#' Missing Coordinates
#'
#' @description \code{cs_missingCoords} compares X and Y coordinates and adds a
#'     TRUE/FALSE column that states which rows are missing coordinate
#'     data.
#'
#' @usage cs_missing_xy(.data, x, y)
#'
#' @param .data a data frame
#' @param x the column from the data frame containing the x coordinates
#' @param y the column from the data frame containg the y coordinates
#'
#' @return returns a logical vector that displays TRUE when the coordinates are
#'     missing and FALSE if the coordinates are not missing
#'
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#' @export
cs_missing_xy <- function(.data, x, y){

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

  ifelse(.data[,xN] == 0 & .data[,yN] == 0,"TRUE","FALSE")
}
