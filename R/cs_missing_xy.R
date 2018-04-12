#' Missing Coordinates
#'
#' @description \code{cs_missingCoords} compares X and Y coordinates and adds a
#'     TRUE/FALSE column that states which rows are missing coordinate
#'     data.
#'
#' @usage cs_missing_xy(.data, x, y, newVar)
#'
#' @param .data a data frame
#' @param x the column from the data frame containing the x coordinates
#' @param y the column from the data frame containg the y coordinates
#' @param newVar the name of the new column
#'
#' @return returns a logical vector that displays TRUE when the coordinates are
#'     missing and FALSE if the coordinates are not missing
#'
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#'
#' @export
cs_missing_xy <- function(.data, x, y, newVar){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be separated must be specified for .data')
  }

  if (missing(x)) {
    stop('The column containing the data to be separated must be specified for variable1')
  }

  if (missing(y)) {
    stop('The column containing the data to be separated must be specified for variable2')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  xN <- rlang::quo_name(rlang::enquo(x))
  yN <- rlang::quo_name(rlang::enquo(y))

  if (!is.character(paramList$x)) {
    varx <- rlang::enquo(x)
  } else if (is.character(paramList$x)) {
    varx <- rlang::quo(!! rlang::sym(x))
  }

  if (!is.character(paramList$y)) {
    vary <- rlang::enquo(y)
  } else if (is.character(paramList$y)) {
    vary <- rlang::quo(!! rlang::sym(y))
  }


.data %>%
  dplyr::mutate(newVar = ifelse((!!varx) == 0 & (!!vary) == 0,"TRUE","FALSE"))
}
