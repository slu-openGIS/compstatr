#' Is Intersection
#' @param .data a data frame
#' @param variable the column containing ILEADSAddress
#' @return a logical vector that displays "TRUE" where the column is a intersection and displays "FALSE" when the column isn't an intersection
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name

isIntersection <- function(.data,variable){
  paramList <- as.list(match.call())

  varN <- rlang::quo_name(rlang::enquo(variable))

  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }
 ifelse(.data[,varN] == 0,"TRUE","FALSE")
}
