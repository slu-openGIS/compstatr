#' Seperate Interection
#'
#' @description  Takes a column that contains street addresses and seperates
#'     the addresses into two columns if it is an intersection
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
cs_parse_int <- function(.data,var,newVar1,newVar2){
   # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }
  if (missing(newVar1)) {
    stop('The name of the output column containing a street must be specified for newVar1')
  }

  if (missing(newVar2)) {
    stop('The name of the output column containing a street must be specified for newVar2')
  }

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(var))

  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }
  #separates
  .data %>%
    tidyr::separate((!!var), c(newVar1,newVar2), "/");
}
