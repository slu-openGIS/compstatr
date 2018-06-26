#' Missing Coordinates
#'
#' @description \code{cs_missingCoords} compares X and Y coordinates and adds a
#'     TRUE/FALSE column that states which rows are missing coordinate
#'     data.
#'
#' @usage cs_missing_xy(.data, varx, vary, newVar)
#'
#' @param .data a data frame
#' @param varx the column from the data frame containing the x coordinates
#' @param vary the column from the data frame containg the y coordinates
#' @param newVar the name of the new column
#'
#' @return returns a logical vector that displays TRUE when the coordinates are
#'     missing and FALSE if the coordinates are not missing
#'
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#'
#' @examples
#' cs_filter_crime(january2018,XCoord,Ycoord,IsMissing)
#'
#' @export
cs_missing_xy <- function(.data, varx, vary, newVar){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be separated must be specified for .data')
  }

  if (missing(varx)) {
    stop('The column containing the data to be separated must be specified for varx')
  }

  if (missing(vary)) {
    stop('The column containing the data to be separated must be specified for vary')
  }
  if (missing(newVar)) {
    stop('The output column name must be specified for newVar')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  newVar <- rlang::quo_name(rlang::enquo(newVar))

  xN <- rlang::quo_name(rlang::enquo(varx))
  yN <- rlang::quo_name(rlang::enquo(vary))

  if (!is.character(paramList$varx)) {
    varx <- rlang::enquo(varx)
  } else if (is.character(paramList$varx)) {
    varx <- rlang::quo(!! rlang::sym(varx))
  }

  if (!is.character(paramList$vary)) {
    vary <- rlang::enquo(vary)
  } else if (is.character(paramList$vary)) {
    vary <- rlang::quo(!! rlang::sym(vary))
  }


#create logical vector that is appended to data frame
.data %>%
  dplyr::mutate((!!newVar) := (ifelse((!!varx) == 0 & (!!vary) == 0,"TRUE","FALSE")))
}
