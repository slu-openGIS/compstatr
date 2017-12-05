#" Missing Coordinates
#' missingCoords compares X and Y coordinates and adds a TRUE/FALSE column that states which rows are missing coordinate data
#' @param .data a data frame
#' @param variable1 the column from the data frame containing the x coordinates
#' @param variable2 the column from the data frame containg the y coordinates
#' @return returns the data frame with a new column added to the end that displays TRUE is coordinates are missing and FALSE if the coordinates are not missing
missingCoords <- function(.data,variable1,variable2){#Creates a column named noCoord that displays True or False if X and Y coords equal 0
  paramList <- as.list(match.call())

  if (!is.character(paramList$variabl1e)) {
    var1 <- rlang::enquo(variable1)
  } else if (is.character(paramList$variable1)) {
    var1 <- rlang::quo(!! rlang::sym(variable1))
  }
  if (!is.character(paramList$variable2)) {
    var2 <- rlang::enquo(variable2)
  } else if (is.character(paramList$variable2)) {
    var2 <- rlang::quo(!! rlang::sym(variable2))
  }


  .data %>%
    dplyr::mutate(noCoord = ifelse((!!var1) == 0 & (!!var2) == 0,"TRUE","FALSE"))
}
