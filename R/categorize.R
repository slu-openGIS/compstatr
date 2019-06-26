#' Identify Crimes
#'
#' @description \code{cs_crime} can be used to easily identify
#'     crimes based on a specific single UCR categories or common groupings. This can be used
#'     on any police department's data where codes like \code{31111} (robbery with a firearm)
#'     or \code{142320} (malicious destruction of property) are used to identify crimes.
#'
#' @usage cs_crime(.data, var, newVar, crime)
#'
#' @details The categories used here are derived from the U.S. Federal
#'     Bureau of Investigation's Uniform Crime Reporting codes. Valid inputs for the
#'     \code{crime} argument are as follows:
#'
#' \describe{
#'     \item{\code{"violent"}}{Violent crimes (homicide, rape, aggravated assault, and
#'         robbery)}
#'     \item{\code{"property"}}{Property crimes (burglary, larceny, larceny of a motor
#'         vehicle, and arson)}
#'     \item{\code{"part 1"}}{All violent and property crimes}
#'     \item{\code{"homicide"}}{\code{"murder"} is also acceptable as input as is UCR
#'         code \code{1}}
#'     \item{\code{"rape"}}{\code{"forcible rape"} is also acceptable as input as is
#'         UCR code \code{2}}
#'     \item{\code{"robbery"}}{UCR code \code{3} is also acceptable input}
#'     \item{\code{"agg assualt"}}{\code{"aggravated assualt"} is also acceptable as
#'         input as is UCR code \code{4}}
#'     \item{\code{"burglary"}}{UCR code \code{5} is also acceptable input}
#'     \item{\code{"larceny-theft"}}{\code{"larceny"} and \code{"theft"} are also
#'         acceptable inputs as is UCR code \code{6}}
#'     \item{\code{"mv theft"}}{\code{"motor vehicle theft"}, \code{"motor vehicle
#'         larceny"}, and \code{"mv larceny"} are also acceptable inputs as input
#'         as is UCR code \code{7}}
#'     \item{\code{"arson"}}{UCR code \code{8} is also acceptable input}
#'     \item{\code{"part 2"}}{All other crimes}
#'     \item{\code{"assault"}}{\code{"other assaults"} is also acceptable input as
#'         is UCR code \code{9}}
#'     \item{\code{"forgery"}}{\code{"forgery and counterfeiting"} is also acceptable
#'         input as is UCR code \code{10}}
#'     \item{\code{"fraud"}}{UCR code \code{11} is also acceptable input}
#'     \item{\code{"embezzlement"}}{UCR code \code{12} is also acceptable input}
#'     \item{\code{"stolen prop"}}{\code{"stolen property"} is also acceptable input
#'         as is UCR code \code{13}}
#'     \item{\code{"vandalism"}}{UCR code \code{14} is also acceptable input}
#'     \item{\code{"weapons"}}{UCR code \code{15} is also acceptable input}
#'     \item{\code{"prostitution"}}{\code{"prostitution and commercialized vice"} is
#'         also acceptable input as is UCR code \code{16}}
#'     \item{\code{"sex offenses"}}{UCR code \code{17} is also acceptable input}
#'     \item{\code{"drugs"}}{\code{"drug abuse violations"} is also acceptable input
#'         as is UCR code \code{18}}
#'     \item{\code{"gambling"}}{UCR code \code{19} is also acceptable input}
#'     \item{\code{"family"}}{\code{"offenses against the family and children"} is
#'         also acceptable input as is UCR code \code{20}}
#'     \item{\code{"dwi"}}{\code{"driving under the influence"} is also acceptable
#'         input as is UCR code \code{21}}
#'     \item{\code{"liquor laws"}}{UCR code \code{22} is also acceptable input}
#'     \item{\code{"drunkenness"}}{UCR code \code{23} is also acceptable input}
#'     \item{\code{"discon"}}{\code{"disorderly conduct"} is also acceptable input
#'         as is UCR code \code{24}}
#'     \item{\code{"vagrancy"}}{UCR code \code{25} is also acceptable input}
#'     \item{\code{"other"}}{\code{"all other offenses"} is also acceptable input
#'         as is UCR code \code{26}}
#'     \item{\code{"suspicion"}}{UCR code \code{27} is also acceptable input}
#'     \item{\code{"curfew"}}{\code{"curfew and loitering laws-persons under 18"}
#'         is also acceptable input as is UCR code \code{28}}
#'     \item{\code{"runaway"}}{\code{"runaways-persons under 18"} is also acceptable
#'         input as is UCR code \code{29}}
#' }
#'
#' @param .data A tibble or data frame
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param newVar Name of output variable to be created with logical data
#' @param crime A string describing the crime type to be identified
#'
#' @return A copy of the object with a logical vector that is \code{TRUE} if the given crime matches
#'     the category given in the function.
#'
#' @examples
#' # load example data
#' testData <- january2018
#'
#' # add logical vector for violent crimes
#' testData <- cs_crime(testData, var = crime, newVar = violentCrimes, crime = "violent")
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang sym
#'
#' @export
cs_crime <- function(.data, var, newVar, crime){

  # check for missing parameters
  if (missing(.data)) {
    stop('An existing data frame with integer crime codes must be specified for .data.')
  }

  if (missing(var)) {
    stop("The column containing integer crime codes must be specified for 'var'.")
  }

  if (missing(newVar)) {
    stop("The name of the output variable to be created by the function must be specified for 'newVar'.")
  }

  if (missing(crime)) {
    stop("A string describing the crime type to be identified must be specified for 'crime'.")
  }

  #save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  newVarN <- rlang::quo_name(rlang::enquo(newVar))

  # convert to lower case
  crime <- tolower(crime)

  #Appends the column with the proper crime code

  if (crime == "violent"){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var <= 50000, TRUE, FALSE))

  } else if (crime == "property"){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 50000 & !!var < 90000, TRUE, FALSE))

  } else if (crime == "part 1"){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var <= 90000, TRUE, FALSE))

  } else if (crime == "homicide" | crime == "murder" | crime == 1){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 10000 & !!var < 20000, TRUE, FALSE))

  } else if (crime == "forcible rape" | crime == "rape" | crime == 2){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 20000 & !!var < 30000, TRUE, FALSE))

  } else if (crime == "robbery" | crime == 3){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 30000  & !!var < 40000, TRUE, FALSE))

  } else if (crime == "aggravated assault" | crime == "agg assault" | crime == 4){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 40000 & !!var < 50000, TRUE, FALSE))

  } else if (crime == "burglary" | crime == 5){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 50000 & !!var < 60000, TRUE, FALSE))

  } else if (crime == "larceny-theft" | crime == "larceny" | crime == "theft" | crime == 6){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 60000  & !!var < 70000, TRUE, FALSE))

  } else if (crime == "motor vehicle theft" | crime == "motor vehicle larceny" | crime == "mv theft" | crime == "mv larceny" | crime == 7){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 70000 & !!var < 80000, TRUE, FALSE))

  } else if (crime == "arson" | crime == 8){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 80000 & !!var < 90000, TRUE, FALSE))

  } else if (crime == "part 2" | crime == "part ii"){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 90000, TRUE, FALSE))

  } else if (crime == "other assaults" | crime == "assault" | crime == 9){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 90000 & !!var < 100000, TRUE, FALSE))

  } else if (crime == "forgery and counterfeiting" | crime == "forgery" | crime == 10){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 100000 & !!var < 110000, TRUE, FALSE))

  } else if (crime == "fraud" | crime == 11){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 110000 & !!var < 120000, TRUE, FALSE))

  } else if (crime == "embezzlement" | crime == 12){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 120000 & !!var < 130000, TRUE, FALSE))

  } else if (crime == "stolen property" | crime == "stolen prop" | crime == 13){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 130000 & !!var < 140000, TRUE, FALSE))

  } else if (crime == "vandalism" | crime == 14){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 140000 & !!var < 150000, TRUE, FALSE))

  } else if (crime == "weapons" | crime == 15){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 150000 & !!var < 160000, TRUE, FALSE))

  } else if (crime == "prostitution and commercialized vice" | crime == "prostitution" | crime == 16){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 160000 & !!var < 170000, TRUE, FALSE))

  } else if (crime == "sex offenses" | crime == 17){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 170000 & !!var < 180000, TRUE, FALSE))

  } else if (crime == "drug abuse violations" | crime == "drugs" | crime == 18){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 180000 & !!var < 190000, TRUE, FALSE))

  } else if (crime == "gambling" | crime == 19){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 190000 & !!var < 200000, TRUE, FALSE))

  } else if (crime == "offenses against the family and children" | crime == "family" | crime == 20){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 200000 & !!var < 210000, TRUE, FALSE))

  } else if (crime == "driving under the influence" | crime == "dwi" | crime == 21){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 210000 & !!var < 220000, TRUE, FALSE))

  } else if (crime == "liquor laws" | crime == 22){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 220000 & !!var < 230000, TRUE, FALSE))

  } else if (crime == "drunkenness" | crime == 23){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 230000 & !!var < 240000, TRUE, FALSE))

  } else if (crime == "disorderly conduct" | crime == "discon" | crime == 24){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 240000 & !!var < 250000, TRUE, FALSE))

  } else if (crime == "vagrancy" | crime == 25){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 250000 & !!var < 260000, TRUE, FALSE))

  } else if (crime == "all other offenses" | crime == "other" | crime == 26){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 260000 & !!var < 270000, TRUE, FALSE))

  } else if (crime == "suspicion" | crime == 27){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 270000 & !!var < 280000, TRUE, FALSE))

  } else if (crime == "curfew and loitering laws-persons under 18" | crime == "curfew" | crime == 28){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 280000 & !!var < 290000, TRUE, FALSE))

  } else if (crime == "runaways-persons under 18" | crime == "runaways" | crime == 29){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 290000 & !!var < 300000, TRUE, FALSE))

  }

  # return output
  return(cleanData)

}


#' Categorize Crime
#'
#' @description The SLMPD data contains 5 or 6 digit codes to refer to
#'     specific categories of crime. \code{cs_crime_cat} transforms these
#'     into either string, factor, or simplified numeric categories
#'     like "murder" or "aggravated assault". This can be used
#'     on any police department's data where codes like \code{31111} (robbery with a firearm)
#'     or \code{142320} (malicious destruction of property) are used to identify crimes.
#'
#' @details The categories used here are derived from the U.S. Federal
#'     Bureau of Investigation's Uniform Crime Reporting codes.
#'
#' @usage cs_crime_cat(.data, var, newVar, output)
#'
#' @param .data A tibble or data frame
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param newVar Name of output variable to be created with simplified categories
#' @param output Type of output - either \code{"string"}, \code{"factor"}, or \code{"numeric"}.
#'     If \code{"numeric"} is selected, the general UCR code will be returned (i.e. \code{1} for
#'     homicide, \code{3} for aggravated assault, etc.). Factor output will be returned in order
#'     of descending UCR code (i.e. beginning with homicide, which has a UCR code of \code{1}).
#'
#' @return A copy of the object with the new output variable appended to it.
#'
#' @examples
#' # load example data
#' testData <- january2018
#'
#' # apply categories
#' testData <- cs_crime_cat(testData,var = crime, newVar = crimeCat, output = "numeric")
#'
#' # preview categories
#' table(testData$crimeCat)
#'
#' # apply categories
#' testData <- cs_crime_cat(testData,var = crime, newVar = crimeCat, output = "factor")
#'
#' # preview categories
#' table(testData$crimeCat)
#'
#' # apply categories
#' testData <- cs_crime_cat(testData,var = crime, newVar = crimeCat, output = "string")
#'
#' # preview categories
#' table(testData$crimeCat)
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang is_scalar_character
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
cs_crime_cat <- function(.data, var, newVar, output = c("string", "factor", "numeric")){

  # check for missing parameters
  if (missing(.data)) {
    stop('An existing data frame with integer crime codes must be specified for .data.')
  }

  if (missing(var)) {
    stop("The column containing integer crime codes must be specified for 'var'.")
  }

  if (missing(newVar)) {
    stop("The name of the output variable to be created by the function must be specified for 'newVar'.")
  }

  if (missing(output)) {
    stop("The type of output must be defined. Options are either 'string', 'factor', or 'numeric'.")
  }

  # check for incorrect parameters
  if (rlang::is_scalar_character(output) == FALSE){
    stop("The output type must be a character scalar. Select one of 'string', 'factor', or 'numeric'.")
  }

  if (output %in% c("string", "factor", "numeric") == FALSE){
    stop("The output type must be a character scalar. Select one of 'string', 'factor', or 'numeric'.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  varN <- rlang::quo_name(rlang::enquo(var))

  if (!is.character(paramList$newVar)) {
    varN <- rlang::enquo(newVar)
  } else if (is.character(paramList$newVar)) {
    varN <- rlang::quo(!! rlang::sym(newVar))
  }

  newVarN <- rlang::quo_name(rlang::enquo(newVar))

  #Adds a new column with the numeric, string, or factor name
  if (output == "string" | output == "factor"){

    cleanData <- .data %>%
      dplyr::mutate(!!newVarN := case_when(
        !!var >= 10000 & !!var < 20000 ~ "Homicide",
        !!var >= 20000 & !!var < 30000 ~ "Rape",
        !!var >= 30000 & !!var < 40000 ~ "Robbery",
        !!var >= 40000 & !!var < 50000 ~ "Aggravated Assault",
        !!var >= 50000 & !!var < 60000 ~ "Burgalry",
        !!var >= 60000 & !!var < 70000 ~ "Larceny",
        !!var >= 70000 & !!var < 80000 ~ "Motor Vehicle Theft",
        !!var >= 80000 & !!var < 90000 ~ "Arson",
        !!var >= 90000 & !!var < 100000 ~ "Other Assaults",
        !!var >= 100000 & !!var < 110000 ~ "Forgery and Counterfeiting",
        !!var >= 110000 & !!var < 120000 ~ "Fraud",
        !!var >= 120000 & !!var < 130000 ~ "Embezzlement",
        !!var >= 130000 & !!var < 140000 ~ "Stolen Property",
        !!var >= 140000 & !!var < 150000 ~ "Vandalism",
        !!var >= 150000 & !!var < 160000 ~ "Weapons",
        !!var >= 160000 & !!var < 170000 ~ "Prostitution and Commercialized Vice",
        !!var >= 170000 & !!var < 180000 ~ "Sex Offenses",
        !!var >= 180000 & !!var < 190000 ~ "Drug Abuse Violations",
        !!var >= 190000 & !!var < 200000 ~ "Gambling",
        !!var >= 200000 & !!var < 210000 ~ "Offenses Against the Family and Children",
        !!var >= 210000 & !!var < 220000 ~ "Liquor Laws",
        !!var >= 220000 & !!var < 230000 ~ "Drunkeness",
        !!var >= 230000 & !!var < 240000 ~ "Disorderly Conduct",
        !!var >= 240000 & !!var < 250000 ~ "Vagrancy",
        !!var >= 250000 & !!var < 260000 ~ "All Other Offenses",
        !!var >= 260000 & !!var < 270000 ~ "Suspicion",
        !!var >= 270000 & !!var < 280000 ~ "Curfew and Loitering Laws-Persons under 18",
        !!var >= 280000 & !!var < 290000 ~ "Runaways-Persons under 18"))

    if (output == "factor"){

      cleanData %>%
        dplyr::mutate(!!newVarN := as.factor(!!varN)) %>%
        dplyr::mutate(!!newVarN := factor(!!varN, levels = c("Homicide", "Rape", "Robbery", "Aggravated Assault", "Burgalry",
                                                             "Larceny", "Motor Vehicle Theft",  "Arson", "Other Assaults",
                                                             "Forgery and Counterfeiting", "Fraud", "Embezzlement", "Stolen Property",
                                                             "Vandalism", "Weapons", "Prostitution and Commercialized Vice", "Sex Offenses",
                                                             "Drug Abuse Violations", "Gambling", "Offenses Against the Family and Children",
                                                             "Liquor Laws", "Drunkeness", "Disorderly Conduct", "Vagrancy",
                                                             "All Other Offenses", "Suspicion", "Curfew and Loitering Laws-Persons under 18",
                                                             "Runaways-Persons under 18"))) -> cleanData

    }

  } else if (output == "numeric"){

    cleanData <- .data %>%
      dplyr::mutate(!!newVarN := case_when(
        !!var >= 10000 & !!var < 20000 ~ 1,
        !!var >= 20000 & !!var < 30000 ~ 2,
        !!var >= 30000 & !!var < 40000 ~ 3,
        !!var >= 40000 & !!var < 50000 ~ 4,
        !!var >= 50000 & !!var < 60000 ~ 5,
        !!var >= 60000 & !!var < 70000 ~ 6,
        !!var >= 70000 & !!var < 80000 ~ 7,
        !!var >= 80000 & !!var < 90000 ~ 8,
        !!var >= 90000 & !!var < 100000 ~ 9,
        !!var >= 100000 & !!var < 110000 ~ 10,
        !!var >= 110000 & !!var < 120000 ~ 11,
        !!var >= 120000 & !!var < 130000 ~ 12,
        !!var >= 130000 & !!var < 140000 ~ 13,
        !!var >= 140000 & !!var < 150000 ~ 14,
        !!var >= 150000 & !!var < 160000 ~ 15,
        !!var >= 160000 & !!var < 170000 ~ 16,
        !!var >= 170000 & !!var < 180000 ~ 17,
        !!var >= 180000 & !!var < 190000 ~ 18,
        !!var >= 190000 & !!var < 200000 ~ 19,
        !!var >= 200000 & !!var < 210000 ~ 20,
        !!var >= 210000 & !!var < 220000 ~ 21,
        !!var >= 220000 & !!var < 230000 ~ 22,
        !!var >= 230000 & !!var < 240000 ~ 23,
        !!var >= 240000 & !!var < 250000 ~ 24,
        !!var >= 250000 & !!var < 260000 ~ 25,
        !!var >= 260000 & !!var < 270000 ~ 26,
        !!var >= 270000 & !!var < 280000 ~ 27,
        !!var >= 280000 & !!var < 290000 ~ 28))

  }

  # return output
  return(cleanData)

}
