#''Seperate Coded Month
#'Seperates a column containing coded year and coded month seperated by "-" into two columns and removes the input column
#'@param .data a data frame
#'@param variable the variable containing coded month and coded year
#'@return returns the data frame with two new columns named "codedYear" and "codedMonth" and the input column removed

separateCodedMonth <- function(.data,variable){ #Seperates CodedMonth into two columns and removes input column

  paramList <- as.list(match.call())
  varN <- rlang::quo_name(rlang::enquo(variable))
  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }

  .data %>%
    tidyr::separate((!!var), c("codedYear","codedMonth"), "-", remove = TRUE)
}
