separateDateOccur <- function(.data,variable){#Seperates DateOccur into four columns and removes input column
  paramList <- as.list(match.call())
  varN <- rlang::quo_name(rlang::enquo(variable))
  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }
  .data %>%
    tidyr::separate((!!var), c("monthOccur","dayOccur","yearOccur","timeOccur"), "/|\\ ", remove = TRUE)

}
