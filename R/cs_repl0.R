#'Replace 0
#'Replace0 takes a specified column from the data frame and replaces cells that ahve the value 0 with NA
#' @param .data a data frame
#' @param variable a column from the data frame
#' @return returns the data frame with the 0's in the specified column changed
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang :=

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
