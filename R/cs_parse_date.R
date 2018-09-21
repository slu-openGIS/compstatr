#' Seperate Date Occur
#'
#' @description Creates two columns. One contains month, day, and year and the other contains hour, and minute.
#'
#' @usage cs_parse_date(.data, var, dateVar, timeVar, tz = NULL, keepDateTime = TRUE)
#'
#' @param .data a data frame
#' @param var a column containing month, day, year, and time seperated by "/"
#' @param dateVar Name of variable containing date data
#' @param timeVar Name of variable containing time data
#' @param tz String name of timezone, defaults to system's timezone
#' @param keepDateTime A logical scalar. Keep an intermediate dateTime variable if \code{TRUE}.
#'
#' @return Appends two columns to the data frame. One is the time data and the other is the date data
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom lubridate date
#' @importFrom lubridate parse_date_time
#' @importFrom rlang :=
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @examples
#' testData <- january2018
#' testData <- cs_parse_date(testData, var = DateOccur, dateVar = Date, timeVar = Time)
#'
#'@export
cs_parse_date <- function(.data, var, dateVar, timeVar, tz = NULL, keepDateTime = TRUE){

  # undefined global variables
  dateTime = NULL

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be separated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for var')
  }
  if (missing(dateVar)) {
    stop('The name of the column to be made containing the date information must be specified for dateVar')
  }

  if (missing(timeVar)) {
    stop('The name of the column to be made containing the time information must be specified for timeVar')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

 newDate <- rlang::quo_name(rlang::enquo(dateVar))
 newTime <- rlang::quo_name(rlang::enquo(timeVar))

 #Separates the column by the spacing in the data and returns two columns

  .data %>%
    dplyr::mutate(dateTime := lubridate::parse_date_time(!!var, orders = c("mdy HM"))) %>%
    dplyr::mutate(!!newDate := lubridate::date(dateTime)) %>%
    dplyr::mutate(!!newTime := base::strftime(dateTime, format = "%H:%M:%S")) -> out

  # selectively remove variables
  if (keepDateTime == FALSE){
    .data <- dplyr::select(.data, -c(dateTime))
  }

  return(out)

}

