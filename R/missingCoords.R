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
