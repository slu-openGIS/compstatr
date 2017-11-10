separateIntersection <- function(.data,variable){#Seperates ILEADSStreet into two columns and removes input column. ileadsStreetOne contains street address and ileadsStreetTwo contains street address if ILEADSStreet is an intersection. Replaces blank cells with NA
  paramList <- as.list(match.call())
  varN <- rlang::quo_name(rlang::enquo(variable))
  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }
  .data %>%
    tidyr::separate((!!var), c("ileadsStreetOne","ileadsStreetTwo"), "/");
}
