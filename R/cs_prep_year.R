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
#' \dontrun{
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

  # create vector of filenames
  files <- list.files(path)

  # check number of files
  if (length(files) > 12){

    stop('There are too many files in the specified folder. Load crime files in yearly batches of 12 monthly files.')

  } else if (length(files) < 12){

    warning('There are fewer than 12 files in the specified folder. You are only loading a partial year.')

  }

  # detect html in file extensions
  html <- stringr::str_detect(files, pattern = ".html$")

  # create data frame of files and file extensions
  data <- data.frame(files = files, html = html, stringsAsFactors = FALSE)

  # subset data frame and convert to vector
  data <- dplyr::filter(data, html == TRUE)
  problemFiles <- as.vector(data$files)

  # iterate over each filename, renaming it and coverting to lowercase
  problemFiles %>%
    split(problemFiles) %>%
    purrr::map_chr(~ cs_edit_filename(path = path, file = .x)) -> changes

  # create vector of new filenames
  orignal <- names(changes)
  names(changes) <- NULL

  # create output
  out <- dplyr::as_tibble(data.frame(
    original = orignal,
    new = changes,
    stringsAsFactors = FALSE
  ))

  # return output
  return(out)

}

# edit an individual file name
cs_edit_filename <- function(path, file){

  # construct a new file name that is all lower case, removes .html from end
  newFile <- tolower(stringr::str_replace(file, pattern = ".html$", replacement = ""))

  # create new file paths, adding a forward slash between path and filename if necessary
  if (stringr::str_detect(path, pattern = "/$") == FALSE){

    filePath <- stringr::str_c(path, "/", file)
    newPath <- stringr::str_c(path, "/", newFile)

  } else {

    filePath <- stringr::str_c(path, file)
    newPath <- stringr::str_c(path, newFile)

  }

  # rename file
  fs::file_move(filePath, newPath)

  # return output
  return(newFile)

}
