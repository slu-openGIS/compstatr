#' Create Year List Object
#'
#' @description \code{cs_load_year} is used to load a set of \code{.csv} files
#'    contained in the given directory. This should be used to load a full
#'    year worth of data.
#'
#' @usage cs_load_year(path)
#'
#' @param path A file path
#'
#' @return A year list object containing 12 tibbles - one per month - worth
#'    of crime data.
#'
#' @importFrom purrr map
#' @importFrom readr read_csv
#'
#' @export
cs_load_year <- function(path){

  # create list of all files at path that are csv
  files <- dir(path = path, pattern = "*.csv")

  # read csv files into year list objects
  data <- files %>%
    map(~ suppressMessages(suppressWarnings(read_csv(file.path(path, .)))))

  # return year list object
  return(data)
}
