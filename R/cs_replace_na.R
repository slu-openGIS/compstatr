#' Replace Zeros with NA
#'
#' @description This function a specified column from the data frame and
#'     replaces cells that ahve the value 0 with NA
#'
#' @usage cs_replace_na(.data, variable)
#'
#' @param .data A tbl
#'
#' @param variable a column from the data frame
#'
#' @return returns the data frame with the 0's in the specified column changed
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#'
#' @export
cs_replace_na <- function(.data, variable){

  # save parameters to lis
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(variable)) {
    stop('The column containing the data to be separated must be specified for variable')
  }

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(variable))

  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }

  .data %>%
    dplyr::mutate(!!varN := ifelse((!!var) == 0, NA, (!!var)))
}
