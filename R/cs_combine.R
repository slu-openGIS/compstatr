#' Combine Multiple Years of Data
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
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom lubridate year
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang sym
#'
#' @export
cs_combine <- function(type, date, ...){

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$date)) {
    dateF <- rlang::enquo(date)
  } else if (is.character(paramList$date)) {
    dateF <- rlang::quo(!! rlang::sym(date))
  }

  if (type == "year"){

    # combine listed objects
    results <- dplyr::bind_rows(...)

    # create temporary date varible, filter based on supplied year, then arrange
    results %>%
      cs_parse_date(var = DateOccur, dateVar = date, timeVar = time, keepDateTime = TRUE) %>%
      dplyr::filter(year(date) == !!dateF) %>%
      dplyr::arrange(dateTime) %>%
      dplyr::select(-date, -time, -dateTime) -> results

  }

  return(results)

}
