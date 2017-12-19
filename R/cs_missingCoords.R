#" Missing Coordinates
#' missingCoords compares X and Y coordinates and adds a TRUE/FALSE column that states which rows are missing coordinate data
#' @param .data a data frame
#' @param variable1 the column from the data frame containing the x coordinates
#' @param variable2 the column from the data frame containg the y coordinates
#' @return returns a logical vector that displays TRUE when the coordinates are missing and FALSE if the coordinates are not missing
#'@importFrom rlang quo
#'@importFrom rlang enquo
#'@importFrom rlang quo_name
#'@export
missingCoords <- function(.data,variable1,variable2){
#'  # check for missing parameters
if (missing(.data)) {
  stop('A existing data frame with data to be separated must be specified for .data')
}

if (missing(variable1)) {
  stop('The column containing the data to be separated must be specified for variable1')
}

if (missing(variable1)) {
  stop('The column containing the data to be separated must be specified for variable2')
}

  # save parameters to list
  paramList <- as.list(match.call())
  #quote input variables
  varN1 <- rlang::quo_name(rlang::enquo(variable1))
  varN2 <- rlang::quo_name(rlang::enquo(variable2))
  ifelse(.data[,varN1] == 0 & .data[,varN2] == 0,"TRUE","FALSE")
}
