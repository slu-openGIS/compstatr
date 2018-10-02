#' Combine Multiple Years of Data
#'
#' @description This function...
#'
#' @usage cs_combine(type, date, ...)
#'
#' @param type One of either "year" or "ytd"
#' @param date For \code{type = "year"}, this should be the year of data to be returned.
#'     For \code{type = "ytd"}, this should be the last month to be included in each estimate.
#' @param ... An unquoted list of objects
#'
#' @return A tibble containing a selection of combined crime data for a given time period.
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

  # undefined global variables
  DateOccur = time = dateTime = NULL

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
