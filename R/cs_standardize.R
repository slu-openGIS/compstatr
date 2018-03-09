#'
cs_standardize <- function(.data, month, config = 18){
  
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


