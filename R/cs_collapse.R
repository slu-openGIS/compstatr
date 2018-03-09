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
#' @importFrom dplyr bind_rows
#'
#' @export
cs_collapse <- function(.data){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  # extract each month to an individual object
  jan <- cs_extract_month(.data, month = 1)
  feb <- cs_extract_month(.data, month = 2)
  mar <- cs_extract_month(.data, month = 3)
  apr <- cs_extract_month(.data, month = 4)
  may <- cs_extract_month(.data, month = 5)
  june <- cs_extract_month(.data, month = 6)
  july <- cs_extract_month(.data, month = 7)
  aug <- cs_extract_month(.data, month = 8)
  sept <- cs_extract_month(.data, month = 9)
  oct <- cs_extract_month(.data, month = 10)
  nov <- cs_extract_month(.data, month = 11)
  dec <- cs_extract_month(.data, month = 12)

  # combine each object
  year <- dplyr::bind_rows(jan, feb, mar, apr, may, june, july, aug, sept, oct, nov, dec)

  # return combined object
  return(year)

}
