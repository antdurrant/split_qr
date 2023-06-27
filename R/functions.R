# remotes::install_github("brianwdavis/quadrangle")
library(quadrangle)
library(pdftools)
library(purrr)
library(magick)
library(dplyr)
library(tidyr)
library(stringr)


qr_pdf <- function(path, expected_side = c("top", "bottom")){
  expected_side <- expected_side[1]
  on.exit(gc())
  
  tmp <- fs::path_dir(path)
  # read in
  x <- magick::image_read_pdf(path, density = 72)
  
  # get width
  ht <- magick::image_info(x)$height
  
  # get qr info
  scan <- quadrangle::qr_scan(x)
  
  qr_content <-
    scan$values$value |> 
    paste0(collapse = "") |>
    stringr::str_remove_all('"')
  
  # rotate if qr is on the expected side of the page ----
  # overwrite original pdf file
  # only read at high density if you know you are going to rotate
  # goes very slow otherwise
  
  side <- if(expected_side[1] == "top"){
    min(scan$points$y)  
  } else {
    max(scan$points$y)
  }
  
  
  if(
    expected_side == "top" & side > (ht/2) |
    expected_side == "bottom" & side < (ht/2)
  ){
    magick::image_read(path, density = 300) |> 
      magick::image_rotate(180) |>
      magick::image_write(path)
  }
  
  
  qr_content
}


split_then_qr_scan <- function(f, expected_side = "top"){
  
  tmp <- tempdir()
  # get files and names
  data.frame(
    f = f,
    root =  fs::path_ext_remove(fs::path_file(f)) 
  ) |>
    # split pdf
    dplyr::mutate(
      split_path = 
        purrr::map(
          f, 
          ~qpdf::pdf_split(
            .x, 
            output = fs::path(tmp, root)
          )
        )
    ) |> 
    tidyr::unnest(split_path) |>
    # read qr
    dplyr::mutate(
      qr_text = purrr::map_chr(split_path, ~qr_pdf(.x, expected_side = expected_side))
    )
  
}


combine_by_student_id <- function(x){
  z <- 
    x |>
    dplyr::mutate(student_id = stringr::str_extract(qr_text, "\\d{7}")) |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(
      combined_file = fs::path(unique(fs::path_dir(split_path)),  paste0(unique(student_id), ".pdf")),
      files_to_combine = list(split_path),
      combined = purrr::map2_chr(files_to_combine, combined_file, pdf_combine)
    ) 
  file.remove(x$split_path)
  z
}
