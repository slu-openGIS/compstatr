#' Extract Month from Year List Object
#'
#' @description This function extracts a given month from a list containing 12
#'    tibbles - one per month - for additional data cleaning prior to collapsing
#'    the list. Since months are ordered alphabetically in the year list objects,
#'    this function makes the process of extracting a particular month more
#'    intuitive.
#'
#' @usage cs_extract_month(.data, month)
#'
#' @param .data A list containing monthly crime data
#'
#' @param month A string name or abbreviation of a month, or its numeric value.
#'    Acceptable inputs include, for example, "January", "january", "Jan",
#'    "jan", and 1.
#'
#' @return A tibble containing a single month worth of crime data.
#'
#' @export
cs_extract_month <- function(.data, month){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(month)) {
    stop('The month to be extracted must be specified.')
  }

  # identify input month
  if (month == "January" | month == "Jan" | month == "january" | month == "jan" | month == 1){
    val <- 5
  } else if (month == "February" | month == "Feb" | month == "february" | month == "feb" | month == 2){
    val <- 4
  } else if (month == "March" | month == "Mar" | month == "march" | month == "mar" | month == 3){
    val <- 8
  } else if (month == "April" | month == "Apr" | month == "april" | month == "apr" | month == 4){
    val <- 1
  } else if (month == "May" | month == "may" | month == 5){
    val <- 9
  } else if (month == "June" | month == "Jun" | month == "june" | month == "jun" | month == 6){
    val <- 7
  } else if (month == "July" | month == "Jul" | month == "july" | month == "jul" | month == 7){
    val <- 6
  } else if (month == "August" | month == "Aug" | month == "august" | month == "aug" | month == 8){
    val <- 2
  } else if (month == "September" | month == "Sept" | month == "Sep" | month == "september"
             | month == "sept" | month == "sep" | month == 9){
    val <- 12
  } else if (month == "October" | month == "Oct" | month == "october" | month == "oct" | month == 10){
    val <- 11
  } else if (month == "November" | month == "Nov" | month == "november" | month == "nov" | month == 11){
    val <- 10
  } else if (month == "December" | month == "Dec" | month == "december" | month == "dec" | month == 12){
    val <- 3
  } else {
    stop("The given argument for month does not match an acceptible input.")
  }

  monthData <- .data[[val]]

  return(monthData)

}
