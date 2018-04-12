#' Validate Variable Classes
#'
#' @description This function can be applied to a single month within a year
#'     list object to identify problematic variables that are non-standard classes.
#'
#' @details When the data are read into R, they occasionally are read in in ways
#'     that \code{readr}'s parser incorrectly handles. This issues can be identified
#'     using \code{cs_validate_year} as occuring in specific months.
#'     \code{cs_validate_month} is meant to be a follow-up function that gives more
#'     detail about specific variables that need to be addressed.
#'
#' @usage cs_validate_month(.data, month)
#'
#' @param .data A tbl
#' @param month An option string name or abbreviation of a month, or its numeric value.
#'    Acceptable inputs include, for example, "January", "january", "Jan",
#'    "jan", and 1.
#'
#' @seealso cs_validate_year
#'
#' @export
cs_validate_month <- function(.data, month){

  val <- cs_selectMonth(month)

  monthData <- .data[[val]]

  classes <- lapply(monthData, class)

  probVar <- NULL

  if (classes$Complaint == "character") {
    classResult <- TRUE
  } else if (classes$Complaint != "character"){
    classResult <- FALSE
    probVar <- c(probVar, "Complaint")
  }

  if (classes$CodedMonth == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$CodedMonth != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "CodedMonth")
  }

  if (classes$DateOccur == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$DateOccur != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "DateOccur")
  }

  if (classes$FlagCrime == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$FlagCrime != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "FlagCrime")
  }

  if (classes$FlagUnfounded == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$FlagUnfounded != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "FlagUnfounded")
  }

  if (classes$FlagAdministrative == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$FlagAdministrative != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "FlagAdministrative")
  }

  if (classes$Count == "integer") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Count != "integer"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "Count")
  }

  if (classes$FlagCleanup == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$FlagCleanup != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "FlagCleanup")
  }

  if (classes$Crime == "integer") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Crime != "integer"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "Crime")
  }

  if (classes$District == "integer") {
    classResult <- c(classResult, TRUE)
  } else if (classes$District != "integer"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "District")
  }

  if (classes$Description == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Description != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "Description")
  }

  if (classes$ILEADSAddress == "integer") {
    classResult <- c(classResult, TRUE)
  } else if (classes$ILEADSAddress != "integer"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "ILEADSAddress")
  }

  if (classes$ILEADSStreet == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$ILEADSStreet != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "ILEADSStreet")
  }

  if (classes$Neighborhood == "integer") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Neighborhood != "integer"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "Neighborhood")
  }

  if (classes$LocationName == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$LocationName != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "LocationName")
  }

  if (classes$LocationComment == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$LocationComment != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "LocationComment")
  }

  if (classes$CADAddress == "integer") {
    classResult <- c(classResult, TRUE)
  } else if (classes$CADAddress != "integer"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "CADAddress")
  }

  if (classes$CADStreet == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$CADStreet != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "CADStreet")
  }

  if (classes$XCoord == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$XCoord != "numeric"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "XCoord")
  }

  if (classes$YCoord == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$YCoord != "numeric"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "YCoord")
  }

  if (all(classResult) == TRUE){
    return(TRUE)
  } else if (all(classResult) == FALSE){
    warning('Validation warning - the variables listed in the results did not contain the expected variable classes')
    return(probVar)
  }

}
