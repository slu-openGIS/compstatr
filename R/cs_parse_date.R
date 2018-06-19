#' Seperate Date Occur
#'
#' @description Creates two columns. One contains month, day, and year and the other contains hour, and minute.
#'
#' @usage cs_parse_date(.data, var, newDate, newTime)
#'
#' @param .data a data frame
#' @param var a column containing month, day, year, and time seperated by "/"
#' @param newDate column containing month, day, and year
#' @param newTime column containing hour and minute
#'
#' @return Appends two columns to the data frame. One is the time data and the other is the date data
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom tidyr separate
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom lubridate mdy_hm
#'
#' @examples
#' testData <- january2018
#' testData <- cs_parse_date(testData,DateOccur,Date,Time)
#'
#'@export
cs_parse_date <- function(.data,var, newDate,newTime){#Separates DateOccur into four columns and removes input column

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be separated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for var')
  }
  if (missing(newDate)) {
    stop('The name of the column to be made containing the date information must be specified for newDate')
  }

  if (missing(newTime)) {
    stop('The name of the column to be made containing the time information must be specified for newTime')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

 newDate <- rlang::quo_name(rlang::enquo(newDate))
 newTime <- rlang::quo_name(rlang::enquo(newTime))

 #Separates the column by the spacing in the data and returns two columns

  .data %>%
    dplyr::mutate(newVar = lubridate::mdy_hm((!!var))) %>%
    tidyr::separate((!!var), c(newDate,newTime), sep = " ")


}

