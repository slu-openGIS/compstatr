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
#' @usage cs_validate_year(.data,year)
#'
#' @param .data A tbl
#' @param year A string representing the year being checked, e.g. \code{"2008" }
#'
#' @return A tibble with validation results
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
cs_validate_year <- function(.data, year){

  # undefined global variables
  month = oneMonth = valClasses = valMonth = valVars = varCount = x = y = NULL

  val <- c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)
  valStr <- c("-01", "-02", "-03", "-04", "-05", "-06", "-07", "-08", "-09", "-10", "-11", "-12")

  counter <- 1:12

  results <- data.frame(
    month = c(1,2,3,4,5,6,7,8,9,10,11,12),
    monthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    oneMonth = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
    valMonth = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
    varCount = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
    valVars = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
    valClasses = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
    stringsAsFactors = FALSE
  )

  for (i in counter){

    valItem <- val[[i]]
    valStrItem <- valStr[[i]]

    monthData <- .data[[valItem]]

    test <- paste0(year, valStrItem)

    skipCols <- FALSE

    # does each object within the list have the right number of columns?
    if (ncol(monthData) == 20){
      colCount <- TRUE
      code <- length(unique(monthData$CodedMonth)) == 1
    } else if (ncol(monthData) == 18){
      colCount <- FALSE
      code <- length(unique(monthData$MonthReportedtoMSHP)) == 1
    } else if (ncol(monthData) == 26){
      colCount <- FALSE
      code <- length(unique(monthData$`Coded Month`)) == 1
    } else if (ncol(monthData) != 20 & ncol(monthData) != 18 & ncol(monthData) != 26){
      stop('Validation error - number of columns outside of acceptable range.')
    }

    # does each object within the list represent a single month, and have the
    # months been imported correctly?
    if (code == TRUE & ncol(monthData) == 20){

      singleMonth <- TRUE

      if (unique(monthData$CodedMonth) == test){
        correctMonth <- TRUE
      } else if (unique(monthData$CodedMonth) != test){
        correctMonth <- FALSE
      }

    } else if (code == TRUE & ncol(monthData) == 18){

      singleMonth <- TRUE

      if (unique(monthData$MonthReportedtoMSHP) == test){
        correctMonth <- TRUE
      } else if (unique(monthData$MonthReportedtoMSHP) != test){
        correctMonth <- FALSE
      }

    } else if (code == FALSE){
      singleMonth <- FALSE
      correctMonth <- FALSE
    }

    # are the variables named correctly if the item in the list has 20 variables?
    if (ncol(monthData) == 20){

      validVars <- c("Complaint", "CodedMonth", "DateOccur", "FlagCrime", "FlagUnfounded",
                     "FlagAdministrative", "Count", "FlagCleanup", "Crime", "District",
                     "Description", "ILEADSAddress", "ILEADSStreet", "Neighborhood", "LocationName",
                     "LocationComment", "CADAddress", "CADStreet", "XCoord", "YCoord")

      testVars <- colnames(monthData)

      if (all(testVars == validVars) == TRUE) {
        colNames <- TRUE
      } else if (all(testVars == validVars) == FALSE) {
        colNames <- FALSE
      }

    } else if (ncol(monthData) != 20){
      skipCols <- TRUE
      colNames <- NA
    }

    # are the variables in the correct classes?
    if (ncol(monthData) == 20) {

      classes <- lapply(monthData, class)

      if (classes$Complaint == "character") {
        classResult <- TRUE
      } else if (classes$Complaint != "character"){
        classResult <- FALSE
      }

      if (classes$CodedMonth == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$CodedMonth != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$DateOccur == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$DateOccur != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$FlagCrime == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$FlagCrime != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$FlagUnfounded == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$FlagUnfounded != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$FlagAdministrative == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$FlagAdministrative != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$Count == "integer") {
        classResult <- c(classResult, TRUE)
      } else if (classes$Count != "integer"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$FlagCleanup == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$FlagCleanup != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$Crime == "integer") {
        classResult <- c(classResult, TRUE)
      } else if (classes$Crime != "integer"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$District == "integer") {
        classResult <- c(classResult, TRUE)
      } else if (classes$District != "integer"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$Description == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$Description != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$ILEADSAddress == "integer") {
        classResult <- c(classResult, TRUE)
      } else if (classes$ILEADSAddress != "integer"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$ILEADSStreet == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$ILEADSStreet != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$Neighborhood == "integer") {
        classResult <- c(classResult, TRUE)
      } else if (classes$Neighborhood != "integer"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$LocationName == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$LocationName != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$LocationComment == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$LocationComment != "character"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$CADAddress == "integer") {
        classResult <- c(classResult, TRUE)
      } else if (classes$CADAddress != "integer"){
        classResult <- c(classResult, FALSE)
      }

      if (classes$CADStreet == "character") {
        classResult <- c(classResult, TRUE)
      } else if (classes$CADStreet != "character"){
        classResult <- c(classResult, FALSE)
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

      colClasses <- all(classResult)

    } else if (ncol(monthData) != 20){
      skipCols <- TRUE
      colClasses <- NA
    }

    # write results
    results <- dplyr::mutate(results, varCount = ifelse(i == month, colCount, varCount))
    results <- dplyr::mutate(results, oneMonth = ifelse(i == month, singleMonth, oneMonth))
    results <- dplyr::mutate(results, valMonth = ifelse(i == month, correctMonth, valMonth))
    results <- dplyr::mutate(results, valVars = ifelse(i == month, colNames, valVars))
    results <- dplyr::mutate(results, valClasses = ifelse(i == month, colClasses, valClasses))

  }

  results <- dplyr::as_tibble(results)

  # check results
  if (all(results$varCount) != TRUE){
    warning('Validation warning - not all data tables contain the expected 20 variables.')
  }
  if (all(results$oneMonth) != TRUE){
    warning('Validation warning - not all data tables contain a single month worth of data.')
  }
  if (all(results$valMonth) != TRUE){
    warning('Validation warning - not all data tables contain the expected month.')
  }

  if (skipCols == FALSE){
    results %>%
      dplyr::mutate(x = valVars) %>%
      dplyr::mutate(y = valClasses) %>%
      dplyr::mutate(valVars = ifelse(is.na(valVars) == TRUE, FALSE, valVars)) %>%
      dplyr::mutate(valClasses = ifelse(is.na(valClasses) == TRUE, FALSE, valClasses)) -> results

    if (all(results$valVars) != TRUE){
      warning('Validation warning - not all data tables contain the expected variable names.')
    }
    if (all(results$valClasses) != TRUE){
      warning('Validation warning - not all data tables contain the expected variable classes.')
    }

    results %>%
      dplyr::select(-valVars, -valClasses) %>%
      dplyr::rename(valVars = x) %>%
      dplyr::rename(valClasses = y) -> results
  }

  return(results)

}


