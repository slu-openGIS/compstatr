#' Seperate Interection
#'
#' @description  Takes a column that contains street addresses and seperates
#'     the addresses into two columns if it is an intersection
#'
#' @usage cs_parse_int(.data, var)
#'
#' @param .data a data frame
#' @param var the column containing street adresses
#' @param newStreet1 the column containing roads that were marked as intersections and not marked as intersections
#' @param newStreet2 the column containing roads that are only in an intersection
#'
#' @return returns two columns containing street addresses and removes the input column
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
#' testData <- cs_parse_int(testData,ILEADSStreet,Intersection1,Intersection2)
#'
#' @export
cs_parse_int <- function(.data,var,newStreet1,newStreet2){
   # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }
  if (missing(newStreet1)) {
    stop('The name of the output column containing a street must be specified for newVar1')
  }

  if (missing(newStreet2)) {
    stop('The name of the output column containing a street must be specified for newVar2')
  }

  #quote input variables
  newStreet1 <- rlang::quo_name(rlang::enquo(newStreet1))
  newStreet2 <- rlang::quo_name(rlang::enquo(newStreet2))

  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }
  #separates
  .data %>%
    tidyr::separate((!!var), c(newStreet1,newStreet2), "/");
}
