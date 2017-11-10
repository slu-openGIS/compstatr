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
