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

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing year-list object must be specified for .data.')
  }

  if (missing(month)) {
    stop('The month to be standardized must be specified.')
  }

  if (missing(config)) {
    stop('The non-standard configuration, either 18 or 26 must be specified.')
  }

  #quote input variables

  month <- rlang::quo_name(rlang::enquo(month))


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
    # we only need to clean full years of 18 variables

    if (config == 18){

      df <- .data

      df[[1]] <- cs_std18(df[[1]])
      df[[2]] <- cs_std18(df[[2]])
      df[[3]] <- cs_std18(df[[3]])
      df[[4]] <- cs_std18(df[[4]])
      df[[5]] <- cs_std18(df[[5]])
      df[[6]] <- cs_std18(df[[6]])
      df[[7]] <- cs_std18(df[[7]])
      df[[8]] <- cs_std18(df[[8]])
      df[[9]] <- cs_std18(df[[9]])
      df[[10]] <- cs_std18(df[[10]])
      df[[11]] <- cs_std18(df[[11]])
      df[[12]] <- cs_std18(df[[12]])

      return(df)

    }

  } else if (!missing(month)) {
    # for cleaning inidivudal months, we need to pull a single month out for cleaning

    val <- cs_selectMonth(month)

    monthData <- .data[[val]]

    if (config == 18){

      cleanData <- cs_std18(monthData)

    } else if (config == 26){

      cleanData <- cs_std26(monthData)

    }else {

      stop("The given argument for 'config' does not match an acceptible input of '18' or '26'.")

    }

    .data[[val]] <- cleanData

    return(.data)

  }

}

# standardize month with 18 variables
cs_std18 <- function(.data){

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

  # clean month
  .data %>%
    dplyr::rename(CodedMonth = MonthReportedtoMSHP) %>%
    dplyr::rename(FlagCrime = NewCrimeIndicator) %>%
    dplyr::rename(FlagUnfounded = UnfoundedCrimeIndicator) %>%
    dplyr::rename(FlagAdministrative = AdministrativeAdjustmentIndicator) %>%
    dplyr::rename(DateOccur = DateOccured) %>%
    dplyr::mutate(Complaint = as.character(NA)) %>%
    dplyr::mutate(FlagCleanup = as.character(NA)) %>%
    dplyr::select(Complaint, CodedMonth, DateOccur, FlagCrime, FlagUnfounded, FlagAdministrative, Count,
           FlagCleanup, Crime, District, Description, ILEADSAddress, ILEADSStreet, Neighborhood,
           LocationName, LocationComment, CADAddress, CADStreet, XCoord, YCoord) -> .data

  return(.data)

}

# standardize month with 26 variables
cs_std26 <- function(.data){

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

  # clean month
  .data %>%
    dplyr::select(-`ILeads Add`, -`ILeads Approve`, -Beat, -`ILeads Asg`,
           -`ILeads Type`, -`Date Crime Coded`) %>%
    dplyr::rename(CodedMonth = `Coded Month`) %>%
    dplyr::rename(DateOccur = `Date Occur`) %>%
    dplyr::rename(FlagCrime = `Flag-Crime`) %>%
    dplyr::rename(FlagUnfounded = `Flag-Unfounded`) %>%
    dplyr::rename(FlagAdministrative = `Flag-Administrative`) %>%
    dplyr::rename(FlagCleanup = `Flag Cleanup`) %>%
    dplyr::rename(ILEADSAddress = `ILEADS-Address`) %>%
    dplyr::rename(ILEADSStreet = `ILEADS-Street`) %>%
    dplyr::rename(LocationName = `Location Name`) %>%
    dplyr::rename(LocationComment = `Location Comment`) %>%
    dplyr::rename(CADAddress = `CAD-Address`) %>%
    dplyr::rename(CADStreet = `CAD-Street`) %>%
    dplyr::rename(XCoord = `X-Coord`) %>%
    dplyr::rename(YCoord = `Y-Coord`) %>%
    dplyr::select(Complaint, CodedMonth, DateOccur, FlagCrime, FlagUnfounded,
           FlagAdministrative, Count, FlagCleanup, Crime, District,
           Description, ILEADSAddress, ILEADSStreet, Neighborhood,
           LocationName, LocationComment, CADAddress, CADStreet,
           XCoord, YCoord) -> .data

  return(.data)

}

# select month for standardization
cs_selectMonth <- function(month){

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

  # return output
  return(val)

}
