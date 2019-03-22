#' Filter Crimes
#'
#' @description \code{cs_filter_crime} can be used to subset based on
#'     specific single UCR categories or common groupings. This can be used
#'     on any police department's data where codes like \code{31111} (robbery with a firearm)
#'     or \code{142320} (malicious destruction of property) are used to identify crimes.
#'
#' @usage cs_filter_crime(.data, var, crime)
#'
#' @details The categories used here are derived from the U.S. Federal
#'     Bureau of Investigation's Uniform Crime Reporting codes. Valid inputs for the
#'     \code{crime} argument are as follows:
#'
#' \describe{
#'     \item{\code{"violent"}}{Violent crimes (homicide, rape, aggrevated assault, and
#'         robbery)}
#'     \item{\code{"property"}}{Property crimes (burglary, larceny, larceny of a motor
#'         vehicle, and arson)}
#'     \item{\code{"part 1"}}{All violent and property crimes}
#'     \item{\code{"homicide"}}{\code{"murder"} is also acceptible as input as is UCR
#'         code \code{1}}
#'     \item{\code{"rape"}}{\code{"forcible rape"} is also acceptible as input as is
#'         UCR code \code{2}}
#'     \item{\code{"robbery"}}{UCR code \code{3} is also acceptible input}
#'     \item{\code{"agg assualt"}}{\code{"aggrevated assualt"} is also acceptible as
#'         input as is UCR code \code{4}}
#'     \item{\code{"burglary"}}{UCR code \code{5} is also acceptible input}
#'     \item{\code{"larceny-theft"}}{\code{"larceny"} and \code{"theft"} are also
#'         acceptible inputs as is UCR code \code{6}}
#'     \item{\code{"mv theft"}}{\code{"motor vehicle theft"}, \code{"motor vehicle
#'         larceny"}, and \code{"mv larceny"} are also acceptible inputs as input
#'         as is UCR code \code{7}}
#'     \item{\code{"arson"}}{UCR code \code{8} is also acceptible input}
#'     \item{\code{"part 2"}}{All other crimes}
#'     \item{\code{"assault"}}{\code{"other assaults"} is also acceptible input as
#'         is UCR code \code{9}}
#'     \item{\code{"forgery"}}{\code{"forgery and counterfeiting"} is also acceptible
#'         input as is UCR code \code{10}}
#'     \item{\code{"fraud"}}{UCR code \code{11} is also acceptible input}
#'     \item{\code{"embezzlement"}}{UCR code \code{12} is also acceptible input}
#'     \item{\code{"stolen prop"}}{\code{"stolen property"} is also acceptible input
#'         as is UCR code \code{13}}
#'     \item{\code{"vandalism"}}{UCR code \code{14} is also acceptible input}
#'     \item{\code{"weapons"}}{UCR code \code{15} is also acceptible input}
#'     \item{\code{"prostitution"}}{\code{"prostitution and commercialized vice"} is
#'         also acceptible input as is UCR code \code{16}}
#'     \item{\code{"sex offenses"}}{UCR code \code{17} is also acceptible input}
#'     \item{\code{"drugs"}}{\code{"drug abuse violations"} is also acceptible input
#'         as is UCR code \code{18}}
#'     \item{\code{"gambling"}}{UCR code \code{19} is also acceptible input}
#'     \item{\code{"family"}}{\code{"offenses against the family and children"} is
#'         also acceptible input as is UCR code \code{20}}
#'     \item{\code{"dwi"}}{\code{"driving under the influence"} is also acceptible
#'         input as is UCR code \code{21}}
#'     \item{\code{"liquor laws"}}{UCR code \code{22} is also acceptible input}
#'     \item{\code{"drunkenness"}}{UCR code \code{23} is also acceptible input}
#'     \item{\code{"discon"}}{\code{"disorderly conduct"} is also acceptible input
#'         as is UCR code \code{24}}
#'     \item{\code{"vagrancy"}}{UCR code \code{25} is also acceptible input}
#'     \item{\code{"other"}}{\code{"all other offenses"} is also acceptible input
#'         as is UCR code \code{26}}
#'     \item{\code{"suspicion"}}{UCR code \code{27} is also acceptible input}
#'     \item{\code{"curfew"}}{\code{"curfew and loitering laws-persons under 18"}
#'         is also acceptible input as is UCR code \code{28}}
#'     \item{\code{"runaway"}}{\code{"runaways-persons under 18"} is also acceptible
#'         input as is UCR code \code{29}}
#' }
#'
#' @param .data A tibble or data frame
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param crime A string describing the crime type to be identified
#'
#' @return A subset object with only the specified crimes
#'
#' @examples
#' # load example data
#' testData <- january2018
#'
#' # subset data to retain only violent crimes
#' testData <- cs_filter_crime(testData, var = Crime, crime = "violent")
#'
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang sym
#'
#' @export
cs_filter_crime <- function(.data, var, crime){

  # check for missing parameters
  if (missing(.data)) {
    stop("A existing data frame with data to be subset must be specified for '.data'.")
  }

  if (missing(var)) {
    stop("The column containing integer crime codes must be specified for 'var'.")
  }

  if (missing(crime)) {
    stop("The crime to be extracted must be specified with 'crime'.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  # convert to lower case
  crime <- tolower(crime)

  #identify input crime
  if (crime == "violent"){

    subsetData <- dplyr::filter(.data, !!var <= 50000)

  } else if (crime == "property"){

    subsetData <- dplyr::filter(.data, !!var >= 50000 & !!var < 90000)

  } else if (crime == "part 1"){

    subsetData <- dplyr::filter(.data, !!var <= 90000)

  } else if (crime == "homicide" | crime == "murder" | crime == 1){

    subsetData <- dplyr::filter(.data, !!var >= 10000 & !!var < 20000)

  } else if (crime == "forcible rape" | crime == "rape" | crime == 2){

    subsetData <- dplyr::filter(.data, !!var >= 20000 & !!var < 30000)

  } else if (crime == "robbery" | crime == 3){

    subsetData <- dplyr::filter(.data, !!var >= 30000 & !!var < 40000)

  } else if (crime == "aggravated assault" | crime == "agg assault" | crime == 4){

    subsetData <- dplyr::filter(.data, !!var >= 40000 & !!var < 50000)

  } else if (crime == "burglary" | crime == 5){

    subsetData <- dplyr::filter(.data, !!var >= 50000 & !!var < 60000)

  } else if (crime == "larceny-theft" | crime == "larceny" | crime == "theft" | crime == 6){

    subsetData <- dplyr::filter(.data, !!var >= 60000 & !!var < 70000)

  } else if (crime == "motor vehicle theft" | crime == "motor vehicle larceny" | crime == "mv theft" | crime == "mv larceny" | crime == 7){

    subsetData <- dplyr::filter(.data, !!var >= 70000 & !!var < 80000)

  } else if (crime == "arson" | crime == 8){

    subsetData <- dplyr::filter(.data, !!var >= 80000 & !!var < 90000)

  } else if (crime == "part 2"){

    subsetData <- dplyr::filter(.data, !!var >= 90000)

  } else if (crime == "other assaults" | crime == "assault" | crime == 9){

    subsetData <- dplyr::filter(.data, !!var >= 90000 & !!var < 100000)

  } else if (crime == "forgery and counterfeiting" | crime == "forgery" | crime == 10){

    subsetData <- dplyr::filter(.data, !!var >= 100000 & !!var < 110000)

  } else if (crime == "fraud" | crime == 11){

    subsetData <- dplyr::filter(.data, !!var >= 110000 & !!var < 120000)

  } else if (crime == "embezzlement" | crime == 12){

    subsetData <- dplyr::filter(.data, !!var >= 120000 & !!var < 130000)

  } else if (crime == "stolen property" | crime == "stolen prop" | crime == 13){

    subsetData <- dplyr::filter(.data, !!var >= 130000 & !!var < 140000)

  } else if (crime == "vandalism" | crime == 14){

    subsetData <- dplyr::filter(.data, !!var >= 140000 & !!var < 150000)

  } else if (crime == "weapons" | crime == 15){

    subsetData <- dplyr::filter(.data, !!var >= 150000 & !!var < 160000)

  } else if (crime == "prostitution and commercialized vice" | crime == "prostitution" | crime == 16){

    subsetData <- dplyr::filter(.data, !!var >= 160000 & !!var < 170000)

  } else if (crime == "sex offenses" | crime == 17){

    subsetData <- dplyr::filter(.data, !!var >= 170000 & !!var < 180000)

  } else if (crime == "drug abuse violations" | crime == "drugs" | crime == 18){

    subsetData <- dplyr::filter(.data, !!var >= 180000 & !!var < 190000)

  } else if (crime == "gambling" | crime == 19){

    subsetData <- dplyr::filter(.data, !!var >= 190000 & !!var < 200000)

  } else if (crime == "offenses against the family and children" | crime == "family" | crime == 20){

    subsetData <- dplyr::filter(.data, !!var >= 200000 & !!var < 210000)

  } else if (crime == "driving under the influence" | crime == "dwi" | crime == 21){

    subsetData <- dplyr::filter(.data, !!var >= 210000 & !!var < 220000)

  } else if (crime == "liquor laws" | crime == 22){

    subsetData <- dplyr::filter(.data, !!var >= 220000 & !!var < 230000)

  } else if (crime == "drunkenness" | crime == 23){

    subsetData <- dplyr::filter(.data, !!var >= 230000 & !!var < 240000)

  } else if (crime == "disorderly conduct" | crime == "discon" | crime == 24){

    subsetData <- dplyr::filter(.data, !!var >= 240000 & !!var < 250000)

  } else if (crime == "vagrancy" | crime == 25){

    subsetData <- dplyr::filter(.data, !!var >= 250000 & !!var < 260000)

  } else if (crime == "all other offenses" | crime == "other" | crime == 26){

    subsetData <- dplyr::filter(.data, !!var >= 260000 & !!var < 270000)

  } else if (crime == "suspicion" | crime == 27){

    subsetData <- dplyr::filter(.data, !!var >= 270000 & !!var < 280000)

  } else if (crime == "curfew and loitering laws-persons under 18" | crime == "curfew" | crime == 28){

    subsetData <- dplyr::filter(.data, !!var >= 280000 & !!var < 290000)

  } else if (crime == "runaways-persons under 18" | crime == "runaway" | crime == 29){

    subsetData <- dplyr::filter(.data, !!var >= 290000)

  } else {

    stop("The given argument for crime does not match an acceptible input.")

  }

  # return output
  return(subsetData)

}

#' Remove Negative Counts
#'
#' @description Removes the row that contains \code{-1} in a specified column, indicating that the
#'     charge described in that observation has either been deemed unfounded or has been
#'     up-coded. For example, a victim of an aggrevated assault dies, and the charge is changed
#'     to homicide.
#'
#' @usage cs_filter_count(.data, var)
#'
#' @param .data A tibble or data frame
#' @param var the name of the column
#'
#' @return A subset object with rows containing \code{-1} removed
#'
#' @examples
#' # load example data
#' testData <- january2018
#'
#' # subset data to remove negative counts
#' testData <- cs_filter_count(testData, var = Count)
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
cs_filter_count <- function(.data, var){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop("A existing data frame with data to be edited must be specified for '.data'.")
  }

  if (missing(var)) {
    stop("The column containing the data to be edited must be specified for 'var'.")
  }

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  # filters for counts of 1
  .data <- dplyr::filter(.data, !!var == 1)

  # return output
  return(.data)

}
