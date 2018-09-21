#' Combine Months in Year List Object
#'
#' @description This function takes a list containing 12 tibbles - one per
#'    month - that have been validated and collapses them into a single tibble.
#'    This is preferable for working with the data post validation, but cannot
#'    be done until there is parity in terms of variable names and classes.
#'
#' @usage cs_collapse(.data)
#'
#' @param .data A list containing monthly crime data
#'
#' @return A tibble containing a year worth of crime data.
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#'
#' @export
cs_collapse <- function(.data){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  # extract each month, collapse, and re-order
  purrr::map(.data, dplyr::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(CodedMonth, DateOccur) -> year

  # add new class
  class(data) <- append(class(data), "cs_collapsed_year")

  # return combined object
  return(year)

}
