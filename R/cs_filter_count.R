#' Remove Negative Counts
#'
#' @description Removes the row that contains -1 in a specified column
#'
#' @details Crimes with a count of -1 are crimes that were determined to be unfounded
#'
#' @usage cs_filter_count(.data, var)
#'
#' @param .data A tbl
#' @param var the name of the column
#'
#' @return returns the data frame with the rows containing -1 removed
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @examples
#' testData <- january2018
#' testData <- cs_filter_count(testData,Count)
#'
#' @export
cs_filter_count <- function(.data,var){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  #Filters for counts of 1
  .data %>%
    dplyr::filter(((!!var)) == 1)
}
