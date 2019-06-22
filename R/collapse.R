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
#'    execution, the following changes are made:
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
#' @examples
#' # load example year-list object
#' load(system.file("testdata", "yearList17.rda", package = "compstatr", mustWork = TRUE))
#'
#' # validate
#' cs_validate(yearList17, year = 2017)
#'
#' # standaridze May, which has 26 variables
#' yearList17 <- cs_standardize(yearList17, month = "May", config = 26)
#'
#' # validate again to confirm fix
#' cs_validate(yearList17, year = 2017)
#'
#' # collapse now that the data are valid
#' crimeReports17 <- cs_collapse(yearList17)
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
  coded_month = date_occur = cad_address = count = crime = district = ileads_address = neighborhood = x_coord = y_coord = NULL

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing year-list object must be specified for .data.')
  }

  # extract each month, collapse, and re-order
  purrr::map(.data, dplyr::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(coded_month, date_occur) -> out

  # reformat variables
  out %>%
    dplyr::mutate(
      count = as.integer(count),
      crime = as.integer(crime),
      district = as.integer(district),
      ileads_address = suppressWarnings(as.numeric(ileads_address)),
      neighborhood = as.integer(neighborhood),
      cad_address = suppressWarnings(as.numeric(cad_address)),
      x_coord = as.numeric(x_coord),
      y_coord = as.numeric(y_coord)
    ) -> out

  # return combined object
  return(out)

}

#' Ensure Objects Contain Data Only For a Given Year
#'
#' @description Since crimes are sometimes reported well after they are committed, objects created
#'     with \code{cs_collapse} often contain crimes that occurred in prior years. The \code{cs_combine}
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
#' @param type "year" is the only valid input currently; year to date functionality
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
#' # load example year-list objects
#' load(system.file("testdata", "yearList17.rda", package = "compstatr", mustWork = TRUE))
#' load(system.file("testdata", "yearList18.rda", package = "compstatr", mustWork = TRUE))
#'
#' # validate
#' cs_validate(yearList17, year = 2017)
#' cs_validate(yearList18, year = 2018)
#'
#' # standaridze May for the 2017 object, which has 26 variables
#' yearList17 <- cs_standardize(yearList17, month = "May", config = 26)
#'
#' # validate again to confirm fix
#' cs_validate(yearList17, year = 2017)
#'
#' # collapse now that the data are valid
#' crimeReports17 <- cs_collapse(yearList17)
#' crimeReports18 <- cs_collapse(yearList18)
#'
#' # combine to add all sample 2017 crimes reported in 2018 to a single 2017 object
#' # and remove from our 2017 object all sample crimes reported in 2017 that occured prior
#' # to that year
#' crime17 <- cs_combine(type = "year", date = 2017, crimeReports17, crimeReports18)
#'
#' @export
cs_combine <- function(type = "year", date, ...){

  # global bindings
  date_occur = time = dateTime = cs_year = ...date = ...time = NULL

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
      cs_parse_date(var = date_occur, dateVar = ...date, timeVar = ...time, keepDateTime = TRUE) %>%
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
