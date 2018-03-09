#' Standardized Variables
#'
#' @description Different time points of SLMPD have different numbers of variables and
#'    different names for those variables that are included in both sets of releases.
#'    This function reformats non-standard configurations to a 20 variable standard.
#'
#' @details For all months prior to 2013 and approximately half of the months during
#'    2013, SLMPD data are released with 18 variables. For one month, May 2017,
#'    the data are released with 26 variables. This function can be used to either
#'    edit an entire year list object or to edit only a specified month within it.
#'    In general, years 2008 through 2012 should be edited en masse while the month
#'    specification can be used to edit the months in 2013 and 2017 that are
#'    non-standard.
#'
#' @usage cs_standardize(.data, month, config = 18)
#'
#' @param .data A tbl
#' @param month An option string name or abbreviation of a month, or its numeric value.
#'    Acceptable inputs include, for example, "January", "january", "Jan",
#'    "jan", and 1.
#' @param config The non-standard configuration, either 18 or 26
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#'
#' @export
cs_standardize <- function(.data, month, config = 18){

  # undefined global variables
  AdministrativeAdjustmentIndicator = Beat = `CAD-Address` = `CAD-Street` =
  CADAddress = CADStreet = `Coded Month` = CodedMonth = Complaint = Count =
  Crime = `Date Crime Coded` = `Date Occur` = DateOccur = DateOccured =
  Description = District = `Flag Cleanup` = `Flag-Administrative` =
  `Flag-Crime` = `Flag-Unfounded` = FlagAdministrative = FlagCleanup =
  FlagCrime = FlagUnfounded = `ILEADS-Address` = `ILEADS-Street` =
  ILEADSAddress = ILEADSStreet = `ILeads Add` = `ILeads Approve` =
  `ILeads Asg` = `ILeads Type` = `Location Comment` = `Location Name` =
  LocationComment = LocationName = MonthReportedtoMSHP = Neighborhood =
  NewCrimeIndicator = UnfoundedCrimeIndicator = `X-Coord` = XCoord =
  `Y-Coord` = YCoord = NULL

  if (missing(month)) {

    if (config == 18){

      .data %>%
        rename(CodedMonth = MonthReportedtoMSHP) %>%
        rename(FlagCrime = NewCrimeIndicator) %>%
        rename(FlagUnfounded = UnfoundedCrimeIndicator) %>%
        rename(FlagAdministrative = AdministrativeAdjustmentIndicator) %>%
        rename(DateOccur = DateOccured) %>%
        mutate(Complaint = as.character(NA)) %>%
        mutate(FlagCleanup = as.logical(NA)) %>%
        select(Complaint, CodedMonth, DateOccur, FlagCrime, FlagUnfounded, FlagAdministrative, Count,
               FlagCleanup, Crime, District, Description, ILEADSAddress, ILEADSStreet, Neighborhood,
               LocationName, LocationComment, CADAddress, CADStreet, XCoord, YCoord) -> cleanData

      return(cleanData)

    }

  } else if (!missing(month)) {

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

    if (config == 18){

      monthData %>%
        rename(CodedMonth = MonthReportedtoMSHP) %>%
        rename(FlagCrime = NewCrimeIndicator) %>%
        rename(FlagUnfounded = UnfoundedCrimeIndicator) %>%
        rename(FlagAdministrative = AdministrativeAdjustmentIndicator) %>%
        rename(DateOccur = DateOccured) %>%
        mutate(Complaint = as.character(NA)) %>%
        mutate(FlagCleanup = as.logical(NA)) %>%
        select(Complaint, CodedMonth, DateOccur, FlagCrime, FlagUnfounded, FlagAdministrative, Count,
               FlagCleanup, Crime, District, Description, ILEADSAddress, ILEADSStreet, Neighborhood,
               LocationName, LocationComment, CADAddress, CADStreet, XCoord, YCoord) -> cleanData

    } else if (config == 26){

      monthData %>%
        select(-`ILeads Add`, -`ILeads Approve`, -Beat, -`ILeads Asg`,
               -`ILeads Type`, -`Date Crime Coded`) %>%
        rename(CodedMonth = `Coded Month`) %>%
        rename(DateOccur = `Date Occur`) %>%
        rename(FlagCrime = `Flag-Crime`) %>%
        rename(FlagUnfounded = `Flag-Unfounded`) %>%
        rename(FlagAdministrative = `Flag-Administrative`) %>%
        rename(FlagCleanup = `Flag Cleanup`) %>%
        rename(ILEADSAddress = `ILEADS-Address`) %>%
        rename(ILEADSStreet = `ILEADS-Street`) %>%
        rename(LocationName = `Location Name`) %>%
        rename(LocationComment = `Location Comment`) %>%
        rename(CADAddress = `CAD-Address`) %>%
        rename(CADStreet = `CAD-Street`) %>%
        rename(XCoord = `X-Coord`) %>%
        rename(YCoord = `Y-Coord`) %>%
        select(Complaint, CodedMonth, DateOccur, FlagCrime, FlagUnfounded,
               FlagAdministrative, Count, FlagCleanup, Crime, District,
               Description, ILEADSAddress, ILEADSStreet, Neighborhood,
               LocationName, LocationComment, CADAddress, CADStreet,
               XCoord, YCoord) -> cleanData

    }

    .data[[val]] <- cleanData

    return(.data)

  }

}


