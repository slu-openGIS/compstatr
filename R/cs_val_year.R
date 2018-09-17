#' Validate Year List Object
#'
#' @description Data from SLMPD are released with a number of problems that
#'     \code{cs_validate_year} is designed to identify.
#'
#' @details Since the data are imported alphabetically, \code{cs_validate_year}
#'     checks to ensure that items are imported in the correct order. It also
#'     checks each month to make sure it contains only one month worth of data.
#'     After ensuring that months are being read correctly, the function also
#'     counts the number of variables in each object. For all months prior to
#'     2013 and approximately half of the months during 2013, SLMPD data are
#'     released with 18 variables. For one month, May 2017, the data are released
#'     with 26 variables. Finally, it makes sure that all variables are being
#'     imported with the correct formatting.
#'
#' @usage cs_val_year(.data, year, verbose)
#'
#' @param .data A tbl
#' @param year A string representing the year being checked, e.g. \code{"2008" }
#' @param verbose A logical scalar. If \code{TRUE}, a full validation report summarizing
#'     results will be returned. If \code{FALSE}, a single value will be returned.
#'
#' @return A tibble with validation results
#'
#' @importFrom purrr map
#'
#' @export
cs_val_year <- function(.data, year, verbose = FALSE){

  # create list of months associated with year list object items
  .data %>%
    purrr::map(cs_checkMonth) -> result

  return(result)

}

#' Check Month of Year List Object for Correct Properties
#'
#' @description  This checks a single month for the correct properties
#'
#' @keywords internal
#'
#' @param monthItem A single item in a year list object
#'
cs_checkMonth <- function(monthItem){

  monthStr <- name(monthItem)
  monthVal <- cs_matchMonthNum(monthStr)

}

#' Match List Name with Numeric Value
#'
#' @description .
#'
#' @keywords internal
#'
#' @param x The last two characters of the first observation's coded month value
#'
cs_matchMonthNum <- function(x){

  if (x == "January") {

    val <- 1

  } else if (x == "February") {

    val <- 2

  } else if (x == "March") {

    val <- 3

  } else if (x == "April") {

    val <- 4

  } else if (x == "May") {

    val <- 5

  } else if (x == "June") {

    val <- 6

  } else if (x == "July") {

    val <- 7

  } else if (x == "August") {

    val <- 8

  } else if (x == "September") {

    val <- 9

  } else if (x == "October") {

    val <- 10

  } else if (x == "November") {

    val <- 11

  } else if (x == "December") {

    val <- 12

  }

  return(val)

}
