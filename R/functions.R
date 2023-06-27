# remotes::install_github("brianwdavis/quadrangle")
library(quadrangle)
library(pdftools)
library(purrr)
library(magick)
library(dplyr)
library(tidyr)
library(stringr)


#' get qr content from pdf (and maybe rotate it)
#'
#' @param path path to file
#' @param expected_side should the qr code be near the top or bottom of the page?
#'
#' @return length 1 character vector of all qr content
#' @export
#'
#' @examples
qr_pdf <- function(path, expected_side = c("top", "bottom")){
  expected_side <- expected_side[1]
  # magick doesn't like letting go once it is out of scope?
  # see https://blog.djnavarro.net/posts/2022-03-17_using-aws-s3-in-r/#a-minor-irritant-appears
  on.exit(gc())
  
  tmp <- fs::path_dir(path)
  # read in
  # density @300 (default) is painfully slow &
  # doesn't seem to be necessary for our usecase
  x <- magick::image_read_pdf(path, density = 72)
  
  # get height
  ht <- magick::image_info(x)$height
  
  # get qr info
  scan <- quadrangle::qr_scan(x)
  
  # smoosh it all into one string
  qr_content <-
    scan$values$value |> 
    paste0(collapse = "") 
  
  # rotate if qr is not in the expected area of the page ----
  # probably should do more validation, but this is simple and should handle
  # the basic use-cases
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


#' split pdf and scan each page for qr code data
#'
#' 
#'
#' @param path path to file 
#' @param expected_side top or bottom - passed on to [qr_pdf()]
#'
#' @return a data.frame with cols path, root, split_path, qr_text
#' @export
#'
#' @examples
split_then_qr_scan <- function(path, expected_side = "top"){
  
  tmp <- tempdir()
  # get files and names
  data.frame(
    path = path,
    root =  fs::path_ext_remove(fs::path_file(f)) 
  ) |>
    # split pdf
    dplyr::mutate(
      split_path = 
        purrr::map(
          path, 
          ~qpdf::pdf_split(.x, output = fs::path(tmp, root)
          )
        )
    ) |> 
    tidyr::unnest(split_path) |>
    # read qr
    dplyr::mutate(
      qr_text = 
        purrr::map_chr(
          split_path, 
          ~qr_pdf(.x, expected_side = expected_side))
    )
  
}


#' combine split pdfs by student
#' 
#' Takes the output of [split_then_qr_scan()] and combines all pages for
#' each student. 
#' 
#' Note that this cleans up the original split pdfs from the temp directory before returning.
#'
#' @param data data.frame returned by [split_then_qr_scan()]
#'
#' @return data.frame with columns `student_id`, `combined_file`, `files_to_combine`, `combined`. `combined_file` & `combined` should be identical - these are the paths to the combined files, to be zipped etc downstream.
#' @export
#'
#' @examples
combine_by_student_id <- function(data){
  x <- 
    data |>
    dplyr::mutate(student_id = stringr::str_extract(qr_text, "\\d{7}")) |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(
      combined_file = fs::path(unique(fs::path_dir(split_path)),  paste0(unique(student_id), ".pdf")),
      files_to_combine = list(split_path),
      combined = purrr::map2_chr(files_to_combine, combined_file, pdf_combine)
    ) 
  file.remove(x$split_path)
  x
}


