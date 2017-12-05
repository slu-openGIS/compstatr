#'' Remove Negative Counts
#'Removes values of -1 from a column
#'@param .data a data frame
#'@param variable the name of the column
#'@return returns the data frame with the rows containing -1 removed
removeNegativeCount <- function(.data,variable){#Removes count equal to -1

  paramList <- as.list(match.call())
  varN <- rlang::quo_name(rlang::enquo(variable))
  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }


  .data %>%
    dplyr::filter(((!!var)) == 1)
}
