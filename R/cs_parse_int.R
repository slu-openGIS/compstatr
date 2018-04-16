#' Seperate Interection
#'
#' @description  Takes a column that contains street addresses and seperates
#'     the addresses into two columns
#'
#' @param .data a data frame
#' @param var the column containing street adresses
#'
#' @return returns two columns containing street addresses and removes the input column
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr separate
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#' @export
cs_parse_int <- function(.data,var){
   # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(var))

  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }
  .data %>%
    tidyr::separate((!!var), c("ileadsStreetOne","ileadsStreetTwo"), "/");
}
