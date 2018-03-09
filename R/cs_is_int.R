#' Is A Location An Intersection?
#'
#' @description The \code{ILEADSAddress} field contains intersections as well
#'     as incidents located at a single street address. This function can be
#'     used to identify intersections for specific operations.
#'
#' @usage cs_isIntersection(.data, variable)
#'
#' @param .data A tbl
#'
#' @param variable A column containing \code{ILEADSAddress} data
#'
#' @return a logical vector that displays \code{TRUE} where the column is a
#'     intersection and displays \code{FALSE} when the column isn't an
#'     intersection.
#'
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#' @export
cs_is_int <- function(.data, variable){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(variable)) {
    stop('The column containing the data to be separated must be specified for variable')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(variable))

  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }

  ifelse(.data[,varN] == 0,"TRUE","FALSE")
}
