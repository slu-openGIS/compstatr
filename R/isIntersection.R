#' Is Intersection
#' @param .data a
#' @param variable

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
