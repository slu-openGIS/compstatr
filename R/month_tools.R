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
#' @seealso \code{\link{cs_replace_month}}
#'
#' @examples
#' \dontrun{
#' jan08 <- cs_extract_month(yearList08, month = 1)
#' jan08 <- cs_extract_month(yearList08, month = "January")
#' }
#'
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#' @export
cs_extract_month <- function(.data, month){


  # check for missing parameters
  if (missing(.data)) {
    stop('A existing year-list object must be specified for .data.')
  }

  if (missing(month)) {
    stop('The month to be extracted must be specified.')
  }

  # quote input variables
  month <- rlang::quo_name(rlang::enquo(month))

  # convert to lower case
  month <- tolower(month)

  # identify input month
  if (month == "january" | month == "jan" | month == 1){
    val <- "January"
  } else if (month == "february" | month == "feb" | month == 2){
    val <- "February"
  } else if (month == "march" | month == "mar" | month == 3){
    val <- "March"
  } else if (month == "april" | month == "apr" | month == 4){
    val <- "April"
  } else if (month == "may" | month == 5){
    val <- "May"
  } else if (month == "june" | month == "jun" | month == 6){
    val <- "June"
  } else if (month == "july" | month == "jul" | month == 7){
    val <- "July"
  } else if (month == "august" | month == "aug" | month == 8){
    val <- "August"
  } else if (month == "september" | month == "sept" | month == "sep" | month == 9){
    val <- "September"
  } else if (month == "october" | month == "oct" | month == 10){
    val <- "October"
  } else if (month == "november" | month == "nov" | month == 11){
    val <- "November"
  } else if (month == "december" | month == "dec" | month == 12){
    val <- "December"
  } else {

    stop("The given argument for month does not match an acceptible input.")

  }

  # create output
  monthData <- .data[[val]]

  # return output
  return(monthData)

}

#' Extract Month from Year-list Object
#'
#' @description This function replaces a single month worth of crime data that
#'   has previously been extracted from a year-list object.
#'
#' @usage cs_replace_month(.data, month, monthData)
#'
#' @param .data A year list object
#' @param month A string name or abbreviation of a month, or its numeric value.
#'    Acceptable inputs include, for example, "January", "january", "Jan",
#'    "jan", and 1.
#' @param monthData A tibble containing a single month worth of crime data.
#'
#' @return An updated year-list object.
#'
#' @seealso \code{\link{cs_extract_month}}
#'
#' \dontrun{
#' yearList08 <- cs_replace_month(yearList08, month = 1, monthData = jan08)
#' yearList08 <- cs_replace_month(yearList08, month = "January", monthData = jan08)
#' }
#'
#' @export
cs_replace_month <- function(.data, month, monthData){

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing year-list object must be specified for .data.')
  }

  if (missing(month)) {
    stop('The month to be replaced must be specified.')
  }

  if (missing(monthData)) {
    stop('The month object to be replaced must be specified.')
  }

  # convert to lower case
  month <- tolower(month)

  if (month == "january" | month == "jan" | month == 1){
    val <- "January"
  } else if (month == "february" | month == "feb" | month == 2){
    val <- "February"
  } else if (month == "march" | month == "mar" | month == 3){
    val <- "March"
  } else if (month == "april" | month == "apr" | month == 4){
    val <- "April"
  } else if (month == "may" | month == 5){
    val <- "May"
  } else if (month == "june" | month == "jun" | month == 6){
    val <- "June"
  } else if (month == "july" | month == "jul" | month == 7){
    val <- "July"
  } else if (month == "august" | month == "aug" | month == 8){
    val <- "August"
  } else if (month == "september" | month == "sept" | month == "sep" | month == 9){
    val <- "September"
  } else if (month == "october" | month == "oct" | month == 10){
    val <- "October"
  } else if (month == "november" | month == "nov" | month == 11){
    val <- "November"
  } else if (month == "december" | month == "dec" | month == 12){
    val <- "December"
  } else {

    stop("The given argument for month does not match an acceptible input.")

  }

  # replace
  .data[[val]] <- monthData

  # return
  return(.data)

}
