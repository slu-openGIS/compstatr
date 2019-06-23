#' Date of Last Crime Data Update from SLMPD
#'
#' @description Data are updated by SLMPD on their crime statistics
#'     site on a monthly basis. This function returns the date of the
#'     last update.
#'
#' @usage cs_last_update(output = "string")
#'
#' @param output A character scalar; if \code{"string"} the date will be
#'     returned in the style of \code{"January 2019"}. If \code{"date"}
#'     the date will be returned as a YYYY-MM-DD date object.
#'
#' @return The date of the last posted data set in the format specified
#'     in the \code{output} parameter.
#'
#' @importFrom rvest html_nodes html_table
#' @importFrom stringr str_length str_sub
#' @importFrom utils head
#' @importFrom xml2 read_html
#'
#' @examples
#' \dontrun{
#' last_update <- cs_last_update()
#' }
#'
#' @export
cs_last_update <- function(output = "string"){

  # read website
  webpage <- xml2::read_html("http://www.slmpd.org/CrimeReport.aspx")

  # store and process first table
  tbl <- rvest::html_nodes(webpage, "table")[[1]]
  tbl <- rvest::html_table(tbl, fill = TRUE)
  tbl <- as.data.frame(tbl)

  # extract vector of months
  months <- tbl$`Crime Detail`
  file <- utils::head(months, 1)

  date <- stringr::str_sub(file, 1, stringr::str_length(file)-4)
  month <- stringr::str_sub(date, 1, stringr::str_length(date)-4)
  year <- stringr::str_sub(date, -4)

  if (output == "string"){

    out <- paste(month, year)

  } else if (output == "date"){

    monthNum <- match(month,month.name)

    out <- as.Date(paste0(year,"-",monthNum,"-",1))

  }

  # return output
  return(out)

}

cs_count_pages <- function(){

  # obtain most recent month
  lastUpdate <- cs_last_update(output = "date")

  # first post was January 2008
  months <- cs_month_nb(lastUpdate) - cs_month_nb("2008-01-01") + 1

  # current page displays 16 months per page
  pages <- months/16

  # round up
  out <- ceiling(pages)

  # return output
  return(out)

}

# https://stackoverflow.com/questions/1995933/number-of-months-between-two-dates/1996404

cs_month_nb <- function(d) {

  # calculate origin date
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))

  # calculate output
  out <- lt$year*12 + lt$mon

  # return output
  return(out)

}
