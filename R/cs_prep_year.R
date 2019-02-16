#' Prepare Raw Data
#'
#' @description Data downloaded from the St. Louis Metropolitan Police Department
#'    are downloaded with incorrect file paths - e.g. \code{January2008.CSV.html}.
#'    This function iterates over all files in a given path and replaces their
#'    file extensions. Thus \code{January2008.CSV.html} will be replaced by
#'    \code{january2008.csv}.
#'
#' @param path File path where raw STLMPD data are located
#'
#' @return A list containing old file names and new file names for reference.
#'
#' @examples
#' \notrun{
#'     cs_prep_year(path = "data/raw/2008")
#' }
#'
#' @importFrom fs file_move
#' @importFrom purrr map
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#'
#' @export
cs_prep_year <- function(path){

  files <- as.vector(list.files(path))

  files %>%
    split(files) %>%
    purrr::map(~cs_edit_filename(path = path, file = .x))

  # new contents
  newFiles <- as.vector(list.files(path))

  # create output
  out <- list(
    original = files,
    new = newFiles
  )

  # return output
  return(out)

}

# edit an individual file name
cs_edit_filename <- function(path, file){

  if (stringr::str_detect(file, pattern = ".html") == TRUE){

    newFile <- tolower(stringr::str_replace(file, pattern = ".html", replacement = ""))

    filePath <- stringr::str_c(path, "/", file)
    newPath <- stringr::str_c(path, "/", newFile)

    fs::file_move(filePath, newPath)

  }

}
