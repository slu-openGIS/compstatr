#' Is A Location An Intersection?
#'
#' @description The \code{ILEADSAddress} field contains intersections as well
#'     as incidents located at a single street address. This function can be
#'     used to identify intersections for specific operations.
#'
#' @usage cs_is_int(.data, variable, newVar)
#'
#' @param .data A tbl
#'
#' @param variable A column containing \code{ILEADSAddress} data
#'
#' @param newVar the name of the column to be created
#'
#' @return a logical vector that displays \code{TRUE} where the column is a
#'     intersection and displays \code{FALSE} when the column isn't an
#'     intersection.
#'
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#'
#' @export
cs_is_int <- function(.data, variable, newVar){

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

  .data %>%
    dplyr::mutate(newVar = (ifelse((!!var) == 0,"TRUE","FALSE")))
}
