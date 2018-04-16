#' Seperate Coded Month
#'
#' @description Seperates a column containing coded year and coded month
#'     seperated by "-" into two columns and removes the input column
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
cs_parse_month <- function(.data,var){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
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

  .data %>%
    tidyr::separate((!var), c("codedYear","codedMonth"), "-", remove = TRUE)
}

