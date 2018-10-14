#' Replace Zeros with NA
#'
#' @description This function a specified column from the data frame and
#'     replaces cells that ahve the value 0 with NA
#'
#' @usage cs_replace_na(.data, var)
#'
#' @param .data A tbl
#' @param var a column from the data frame
#'
#' @return returns the data frame with the 0's in the specified column changed
#'
#' @examples
#' testData <- january2018
#' testData <- cs_replace_na(testData, XCoord)
#' testData <- cs_replace_na(testData, YCoord)
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang sym
#'
#' @export
cs_replace_na <- function(.data, var){

  # save parameters to lis
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be separated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(var))

  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  .data %>%
    dplyr::mutate(!!varN := ifelse((!!var) == 0, NA, (!!var)))
}
