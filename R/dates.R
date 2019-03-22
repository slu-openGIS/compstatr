#' Seperate Coded Month
#'
#' @description Separates a column containing coded year and coded month
#'     separated by "-" into two columns and removes the input column
#'
#' @usage  cs_parse_month(.data,var,newYear,newMonth)
#'
#' @param .data a data frame
#' @param var the variable containing coded month and coded year
#' @param newYear the name of the column to contain the year data
#' @param newMonth the name of the column to contain month data
#'
#' @return returns the data frame with two new columns named "codedYear" and "codedMonth" and the input column removed
#'
#' @examples
#' testData <- january2018
#' testData <- cs_parse_month(testData,CodedMonth,Year,Month)
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr separate
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
cs_parse_month <- function(.data, var, yearVar, monthVar){

  # check for missing parameters
  if (missing(.data)) {
    stop("A existing data frame with data to be parsed must be specified for '.data'.")
  }

  if (missing(var)) {
    stop("The column containing the data to be parsed must be specified for 'var'.")
  }

  if (missing(monthVar)) {
    stop("The name of the new column containing the month must be specified for 'monthVar'.")
  }

  if (missing(yearVar)) {
    stop("The name of the new column containing the year must be specified for 'yearVar'.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  newYear <- rlang::quo_name(rlang::enquo(yearVar))

  newMonth <- rlang::quo_name(rlang::enquo(monthVar))

  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  # separates coded month and year
  .data <- tidyr::separate(.data, (!!var), c(newYear, newMonth), "-", remove = TRUE)

  # return output
  return(.data)

}

#' Seperate Date Occur
#'
#' @description Creates two columns. One contains month, day, and year and the other contains hour, and minute.
#'
#' @usage cs_parse_date(.data, var, dateVar, timeVar, tz = NULL, keepDateTime = TRUE)
#'
#' @param .data a data frame
#' @param var a column containing month, day, year, and time seprated by "/"
#' @param dateVar Name of new column containing date data
#' @param timeVar Name of new column containing time data
#' @param tz String name of timezone, defaults to system's timezone
#' @param keepDateTime A logical scalar. Keep an intermediate dateTime variable if \code{TRUE}.
#'
#' @return Appends two columns to the data frame. One is the time data and the other is the date data
#'
#' @examples
#' testData <- january2018
#' testData <- cs_parse_date(testData, var = DateOccur, dateVar = Date, timeVar = Time)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom lubridate date
#' @importFrom lubridate parse_date_time
#' @importFrom rlang :=
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#'@export
cs_parse_date <- function(.data, var, dateVar, timeVar, tz = NULL, keepDateTime = TRUE){

  # undefined global variables
  dateTime = NULL

  # check for missing parameters
  if (missing(.data)) {
    stop("A existing data frame with data to be parsed must be specified for '.data'.")
  }

  if (missing(var)) {
    stop("The column containing the data to be parsed must be specified for 'var'.")
  }
  if (missing(dateVar)) {
    stop("The name of the new column to be made containing the date must be specified for 'dateVar'.")
  }

  if (missing(timeVar)) {
    stop("The name of the new column to be made containing the time must be specified for 'timeVar'.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  newDate <- rlang::quo_name(rlang::enquo(dateVar))
  newTime <- rlang::quo_name(rlang::enquo(timeVar))

  # separates the column by the spacing in the data and returns two columns
  .data %>%
    dplyr::mutate(dateTime := lubridate::parse_date_time(!!var, orders = c("mdy HM"))) %>%
    dplyr::mutate(!!newDate := lubridate::date(dateTime)) %>%
    dplyr::mutate(!!newTime := base::strftime(dateTime, format = "%H:%M:%S")) -> out

  # optionally remove variables
  if (keepDateTime == FALSE){
    out <- dplyr::select(out, -c(dateTime))
  }

  # return output
  return(out)

}
