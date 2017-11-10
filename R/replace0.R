replace0 <- function(.data, variable){

  paramList <- as.list(match.call())

  varN <- rlang::quo_name(rlang::enquo(variable))

  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }

  .data %>%
    dplyr::mutate(!!varN := ifelse((!!var) == 0, NA, (!!var)))
}
