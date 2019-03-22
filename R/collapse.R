#' Collapse Months in Year List Object into Single Tibble
#'
#' @description This function takes a year-list containing individual tibbles - one per
#'    month - that have been validated and collapses them into a single tibble.
#'
#' @details \code{cs_collapse} applies common sense variable classes to a number
#'    of variables. This is motivated by issues that originate with SLMPD \code{.csv}
#'    files. When they are imported, the \pkg{readr} package sometimes applies
#'    the incorrect variable classes because of formatting issues in the tables.
#'    Since the tables have inconsistent variable names and numbers of variables,
#'    all variables are imported as \code{chr} data. During \code{cs_collapse}'s
#'    exectuion, the following changes are made:
#'
#' \describe{
#'   \item{Count}{Converted to \code{int}}
#'   \item{Crime}{Converted to \code{int}}
#'   \item{District}{Converted to \code{int}}
#'   \item{ILEADSAddress}{Converted to \code{int}}
#'   \item{Neighborhood}{Converted to \code{int}}
#'   \item{CADAddress}{Converted to \code{int}}
#'   \item{XCoord}{Converted to \code{dbl}}
#'   \item{YCoord}{Converted to \code{dbl}}
#' }
#'
#' @usage cs_collapse(.data)
#'
#' @param .data A list containing monthly crime data
#'
#' @return A tibble containing all crime data in a given year-list object.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom purrr map
#'
#' @export
cs_collapse <- function(.data){

  # undefined global variables
  CodedMonth = DateOccur = CADAddress = Count = Crime = District = ILEADSAddress = Neighborhood = XCoord = YCoord = NULL

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing year-list object must be specified for .data.')
  }

  # extract each month, collapse, and re-order
  purrr::map(.data, dplyr::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(CodedMonth, DateOccur) -> out

  # reformat variables
  out %>%
    dplyr::mutate(
      Count = as.integer(Count),
      Crime = as.integer(Crime),
      District = as.integer(District),
      ILEADSAddress = as.integer(ILEADSAddress),
      Neighborhood = as.integer(Neighborhood),
      CADAddress = as.integer(CADAddress),
      XCoord = as.numeric(XCoord),
      YCoord = as.numeric(YCoord)
    ) -> out

  # return combined object
  return(out)

}

#' Ensure Objects Contain Data Only For a Given Year
#'
#' @description Since crimes are somtimes reported well after they are committed, objects created
#'     with \code{cs_collapse} often contain crimes that occured in prior years. The \code{cs_combine}
#'     function ensures that objects contain only data for a given year, with the ability to add in
#'     crimes reported for the given year in later years.
#'
#' @details When applied to a single year's worth of data, \code{cs_combine} will subset out
#'     any crimes that occured in a year other than the one given for the \code{date} argument.
#'
#'     When applied to a range of objects, such as objects for 2017 and 2018, each object will be
#'     subset to identify crimes that occured in the year given for the \code{date} argument. This
#'     creates a more complete accounting of crime in a given year since it adds in crimes reported
#'     in subsequent years to the object. At the same time, crimes that occured prior to the given
#'     year will also be subset out to ensure the resulting object only contains crimes
#'     that occured in that given year.
#'
#' @usage cs_combine(type = "year", date, ...)
#'
#' @param type "year" is the only valid input currenty; year to date functionality
#'     is planned for a later update
#' @param date For \code{type = "year"}, this should be the year of data to be returned.
#'     For \code{type = "ytd"}, this should be the last month to be included in each estimate.
#' @param ... An unquoted list of objects
#'
#' @return A tibble containing a selection of combined crime data for a given time period.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr is.tbl
#' @importFrom dplyr select
#' @importFrom lubridate year
#' @importFrom purrr map_lgl
#' @importFrom rlang is_scalar_character
#' @importFrom rlang list2
#'
#' @examples
#' \dontrun{
#' # remove crimes prior to 2018
#' crimes18 <- cs_combine(type = "year", date = 2018, reported18)
#'
#' # add in crimes reported in 2018 but that occured in 2017 while also
#' # removing from the reported17 object any crimes that were reported
#' # that year but occured in a prior year
#' crimes17 <- cs_combine(type = "year", date = 2017, reported17, reported18)
#' }
#'
#' @export
cs_combine <- function(type = "year", date, ...){

  # global bindings
  ...date = ...time = NULL

  # undefined global variables
  DateOccur = time = dateTime = cs_year = NULL

  # check missing parameters
  if (dplyr::is.tbl(type) == TRUE){
    stop("A timeframe must be given for 'type'. At this time, only 'year' is a valid argument.")
  }

  if (dplyr::is.tbl(date) == FALSE){
    if (class(date) != "numeric"){
      stop("An integer year must be given for 'date'.")
    }
  } else if (dplyr::is.tbl(date) == TRUE){
    stop("An integer year must be given for 'date'.")
  }

  # capture dots
  list <- rlang::list2(...)

  # check for missing tibble
  if (length(list) == 0){
    stop("At least one tibble must be supplied after the 'type' and 'date' arguments.")
  }

  # check dots
  list %>%
    purrr::map_lgl(.f = dplyr::is.tbl) -> tblCheck

  # evaluate result
  if (all(tblCheck) == FALSE){
    stop("Only tibbles containing collapsed objects may be passed through the dots as arguments.")
  }

  # check for incorrect parameters
  if (rlang::is_scalar_character(type) == FALSE){
    stop("The output type must be a character scalar. At this time, only 'year' is a valid argument.")
  } # Select one of 'year' or 'ytd'.

  if (type %in% c("year", "ytd") == FALSE){
    stop("The output type must be a character scalar. At this time, only 'year' is a valid argument.")
  }

  # combine
  if (type == "year"){

    # combine listed objects
    results <- dplyr::bind_rows(...)

    # create temporary date varible, filter based on supplied year, then arrange
    results %>%
      cs_parse_date(var = DateOccur, dateVar = ...date, timeVar = ...time, keepDateTime = TRUE) %>%
      dplyr::filter(year(...date) == date) %>%
      dplyr::arrange(dateTime) %>%
      dplyr::mutate(cs_year = lubridate::year(dateTime)) %>%
      dplyr::select(-...date, -...time, -dateTime) %>%
      dplyr::select(cs_year, dplyr::everything()) -> results

  } else if (type == "ytd"){

    stop("This functionality is still in progress.")

  }

  # return output
  return(results)

}
