#' Seperate Coded Month
#'
#' @description Separates a column containing coded year and coded month
#'     separated by "-" into two columns and removes the input column
#'
#' @usage  cs_parse_month
#'
#' @param .data a data frame
#' @param var the variable containing coded month and coded year
#'
#' @return returns the data frame with two new columns named "codedYear" and "codedMonth" and the input column removed
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr separate
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#' @export
cs_parse_month <- function(.data,var,newVar1,newVar2){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }
  if (missing(newVar1)) {
    stop('The name of the output column containing the month must be specified for newVar1')
  }

  if (missing(newVar2)) {
    stop('The name of the output column containing the year must be specified for newVar2')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(var))
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }
#Separates coded month and year
  .data %>%
    tidyr::separate((!var), c(NewVar1, newVar2), "-", remove = TRUE)
}

