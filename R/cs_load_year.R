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
#' @importFrom dplyr %>%
#'
#' @export
cs_load_year <- function(path){

  # create list of all files at path that are csv
  files <- dir(path = path, pattern = "*.csv")

  # read csv files into year list objects
  files %>%
    map(~ suppressMessages(suppressWarnings(read_csv(file.path(path, .))))) -> data

  # create list of months associated with year list object items
  data %>%
    purrr::map(cs_identifyMonth) -> nameList

  # convert list of months to vector
  nameVector <- unlist(nameList, recursive = TRUE, use.names = TRUE)

  # apply vector to data
  names(data) <- nameVector

  # return year list object
  return(data)
}

#' Extract Month of a Given Year List Object Item
#'
#' @description  This uses the value of the first observation's coded month as the basis for
#'     identifying which month the data are from.
#'
#' @keywords internal
#'
#' @param .data A year list object name
#'
#' @importFrom stringr str_sub
#'
cs_identifyMonth <- function(.data){

  if (length(.data) == 18){

    x <- .data$MonthReportedtoMSHP[1]

  } else if (length(.data) == 20){

    x <- .data$CodedMonth[1]

  } else if (length(.data) == 26){

    x <- .data$`Coded Month`[1]

  }

  x <- stringr::str_sub(x, start = -2)

  x <- cs_matchMonth(x)

  return(x)

}

#' Match Extract Month with Month Name
#'
#' @description Based on the result of cs_identifyMonth, this function returns the
#'     appropriate string name.
#'
#' @keywords internal
#'
#' @param x The last two characters of the first observation's coded month value
#'
cs_matchMonth <- function(x){

  if (x == "01") {

    name <- "January"

  } else if (x == "02") {

    name <- "February"

  } else if (x == "03") {

    name <- "March"

  } else if (x == "04") {

    name <- "April"

  } else if (x == "05") {

    name <- "May"

  } else if (x == "06") {

    name <- "June"

  } else if (x == "07") {

    name <- "July"

  } else if (x == "08") {

    name <- "August"

  } else if (x == "09") {

    name <- "September"

  } else if (x == "10") {

    name <- "October"

  } else if (x == "11") {

    name <- "November"

  } else if (x == "12") {

    name <- "December"

  }

  return(name)

}
