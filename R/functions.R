library(quadrangle)
library(pdftools)
library(purrr)
library(magick)
library(dplyr)
library(tidyr)


qr_pdf <- function(path){
  
  on.exit(gc())
  
  tmp <- fs::path_dir(path)
  # read in
  x <- magick::image_read(path)
  
  # get width
  width <- magick::image_info(x)$width
  
  # get qr info
  scan <- quadrangle::qr_scan(x)
  # make new path
  qr_content <- paste0(tmp, "/", scan$values$value, ".pdf")
  
  # rotate if leftmost point of qr is on the left side of the page
  # overwrite original pdf file
  min_x <- min(scan$points$x)
  
  if(min(min_x) < (width/2)){
    x |>
      magick::image_rotate(180) |>
      magick::image_write(path)
  }
  qr_content
}


split_then_qr_scan <- function(f){
  
  tmp <- tempdir()
  # get files and names
  data.frame(
    f = f,
    root =  tools::file_path_sans_ext(fs::path_file(f)) 
  ) |>
    # split pdf
    dplyr::mutate(
      split_path = 
        purrr::map(
          f, 
          ~pdftools::pdf_split(
            .x, 
            output = fs::path(tmp, root)
          )
        )
    ) |> 
    tidyr::unnest(split_path) |>
    # read qr
    # rename files
    dplyr::mutate(
      new_path = purrr::map_chr(split_path, qr_pdf),
      rewritten = file.rename(split_path, new_path)
    )

}

