#' Extract Month from Year List Object
#'
#' @description This function replaces a single month worth of crime data that
#'   has previously been extracted from a year list object.
#'
#' @usage cs_replace_month(.data, month, monthData)
#'
#' @param .data A year list object
#' @param month A string name or abbreviation of a month, or its numeric value.
#'    Acceptable inputs include, for example, "January", "january", "Jan",
#'    "jan", and 1.
#' @param monthData A tibble containing a single month worth of crime data.
#'
#' @return A tibble containing a single month worth of crime data.
#'
#' @examples
#' testData <- january2018
#' testData <- cs_replace_month(testData, "January", sortedMonth)
#'
#' @export
cs_replace_month <- function(.data, month, monthData){

  if (month == "January" | month == "Jan" | month == "january" | month == "jan" | month == 1){
    val <- "January"
  } else if (month == "February" | month == "Feb" | month == "february" | month == "feb" | month == 2){
    val <- "February"
  } else if (month == "March" | month == "Mar" | month == "march" | month == "mar" | month == 3){
    val <- "March"
  } else if (month == "April" | month == "Apr" | month == "april" | month == "apr" | month == 4){
    val <- "April"
  } else if (month == "May" | month == "may" | month == 5){
    val <- "May"
  } else if (month == "June" | month == "Jun" | month == "june" | month == "jun" | month == 6){
    val <- "June"
  } else if (month == "July" | month == "Jul" | month == "july" | month == "jul" | month == 7){
    val <- "July"
  } else if (month == "August" | month == "Aug" | month == "august" | month == "aug" | month == 8){
    val <- "August"
  } else if (month == "September" | month == "Sept" | month == "Sep" | month == "september"
             | month == "sept" | month == "sep" | month == 9){
    val <- "September"
  } else if (month == "October" | month == "Oct" | month == "october" | month == "oct" | month == 10){
    val <- "October"
  } else if (month == "November" | month == "Nov" | month == "november" | month == "nov" | month == 11){
    val <- "November"
  } else if (month == "December" | month == "Dec" | month == "december" | month == "dec" | month == 12){
    val <- "December"
  } else {

    stop("The given argument for month does not match an acceptible input.")

  }

  .data[[val]] <- monthData

  return(.data)

}
