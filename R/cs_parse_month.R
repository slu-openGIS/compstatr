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
#' @importFrom dplyr %>%
#' @importFrom tidyr separate
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @examples
#' testData <- january2018
#' testData <- cs_parse_month(testData,CodedMonth,Year,Month)
#'
#'
#' @export
cs_parse_month <- function(.data,var,newYear,newMonth){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }
  if (missing(newMonth)) {
    stop('The name of the output column containing the month must be specified for newVar1')
  }

  if (missing(newYear)) {
    stop('The name of the output column containing the year must be specified for newVar2')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  newYear <- rlang::quo_name(rlang::enquo(newYear))
  newMonth <- rlang::quo_name(rlang::enquo(newMonth))
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }
#Separates coded month and year
  .data %>%
    tidyr::separate((!!var), c(newYear, newMonth), "-", remove = TRUE)
}

