#'
cs_validate_month <- function(.data, month){

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
  } else if (month == "September" | month == "Sept" | month == "Sep" | month == "september" | month == "sept" | month == "sep" | month == 9){
    val <- 12
  } else if (month == "October" | month == "Oct" | month == "october" | month == "oct" | month == 10){
    val <- 11
  } else if (month == "November" | month == "Nov" | month == "november" | month == "nov" | month == 11){
    val <- 10
  } else if (month == "December" | month == "Dec" | month == "december" | month == "dec" | month == 12){
    val <- 3
  }

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
  
  if (classes$FlagUnfounded == "logical") {
    classResult <- c(classResult, TRUE)
  } else if (classes$FlagUnfounded != "logical"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "FlagUnfounded")
  }
  
  if (classes$FlagAdministrative == "logical") {
    classResult <- c(classResult, TRUE)
  } else if (classes$FlagAdministrative != "logical"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "FlagAdministrative")
  }
  
  if (classes$Count == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Count != "numeric"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "Count")
  }
  
  if (classes$FlagCleanup == "logical") {
    classResult <- c(classResult, TRUE)
  } else if (classes$FlagCleanup != "logical"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "FlagCleanup")
  }
  
  if (classes$Crime == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Crime != "numeric"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "Crime")
  }
  
  if (classes$District == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$District != "numeric"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "District")
  }
  
  if (classes$Description == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Description != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "Description")
  }
  
  if (classes$ILEADSAddress == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$ILEADSAddress != "numeric"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "ILEADSAddress")
  }
  
  if (classes$ILEADSStreet == "character") {
    classResult <- c(classResult, TRUE)
  } else if (classes$ILEADSStreet != "character"){
    classResult <- c(classResult, FALSE)
    probVar <- c(probVar, "ILEADSStreet")
  }
  
  if (classes$Neighborhood == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$Neighborhood != "numeric"){
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
  
  if (classes$CADAddress == "numeric") {
    classResult <- c(classResult, TRUE)
  } else if (classes$CADAddress != "numeric"){
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
