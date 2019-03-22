#' Filter Crimes
#'
#' @description \code{cs_filter_crime} can be used to subset based on
#'     specific single UCR categories or common groupings.
#'
#' @details The categories used here are derived from the U.S. Federal
#'     Bureau of Investigation's Uniform Crime Reporting codes.
#'
#' @usage cs_filter_crime(.data, var, crime)
#'
#' @param .data A tbl
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param crime A string describing the crime type to be identified
#'
#' @return A subset tibble with only the specified crimes
#'
#' @examples
#' testData <- january2018
#' testData <- cs_filter_crime(testData,Crime,"violent")
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

  } else if (crime == "part 1" | crime == "part i"){

    subsetData <- dplyr::filter(.data, !!var <= 90000)

  } else if (crime == "homicide" | crime == "murder" | crime == 1){

    subsetData <- dplyr::filter(.data, !!var >= 10000 & !!var < 20000)

  } else if (crime == "forcible rape" | crime == "rape" | crime == 2){

    subsetData <- dplyr::filter(.data, !!var >= 20000 & !!var < 30000)

  } else if (crime == "robbery" | crime == 3){

    subsetData <- dplyr::filter(.data, !!var >= 30000 & !!var < 40000)

  } else if (crime == "aggravated assault" | crime == 4){

    subsetData <- dplyr::filter(.data, !!var >= 40000 & !!var < 50000)

  } else if (crime == "burglary" | crime == 5){

    subsetData <- dplyr::filter(.data, !!var >= 50000 & !!var < 60000)

  } else if (crime == "larceny-theft" | crime == 6){

    subsetData <- dplyr::filter(.data, !!var >= 60000 & !!var < 70000)

  } else if (crime == "motor vehicle theft" | crime == 7){

    subsetData <- dplyr::filter(.data, !!var >= 70000 & !!var < 80000)

  } else if (crime == "arson" | crime == 8){

    subsetData <- dplyr::filter(.data, !!var >= 80000 & !!var < 90000)

  } else if (crime == "part 2" | crime == "part ii"){

    subsetData <- dplyr::filter(.data, !!var >= 90000)

  } else if (crime == "other assaults" | crime == 9){

    subsetData <- dplyr::filter(.data, !!var >= 90000 & !!var < 100000)

  } else if (crime == "forgery and counterfeiting" | crime == 10){

    subsetData <- dplyr::filter(.data, !!var >= 100000 & !!var < 110000)

  } else if (crime == "fraud" | crime == 11){

    subsetData <- dplyr::filter(.data, !!var >= 110000 & !!var < 120000)

  } else if (crime == "embezzlement" | crime == 12){

    subsetData <- dplyr::filter(.data, !!var >= 120000 & !!var < 130000)

  } else if (crime == "stolen property" | crime == 13){

    subsetData <- dplyr::filter(.data, !!var >= 130000 & !!var < 140000)

  } else if (crime == "vandalism" | crime == 14){

    subsetData <- dplyr::filter(.data, !!var >= 140000 & !!var < 150000)

  } else if (crime == "weapons" | crime == 15){

    subsetData <- dplyr::filter(.data, !!var >= 150000 & !!var < 160000)

  } else if (crime == "prostitution and commercialized vice" | crime == 16){

    subsetData <- dplyr::filter(.data, !!var >= 160000 & !!var < 170000)

  } else if (crime == "sex offenses" | crime == 17){

    subsetData <- dplyr::filter(.data, !!var >= 170000 & !!var < 180000)

  } else if (crime == "drug abuse violations" | crime == 18){

    subsetData <- dplyr::filter(.data, !!var >= 180000 & !!var < 190000)

  } else if (crime == "gambling" | crime == 19){

    subsetData <- dplyr::filter(.data, !!var >= 190000 & !!var < 200000)

  } else if (crime == "offenses against the family and children" | crime == 20){

    subsetData <- dplyr::filter(.data, !!var >= 200000 & !!var < 210000)

  } else if (crime == "driving under the influence" | crime == 21){

    subsetData <- dplyr::filter(.data, !!var >= 210000 & !!var < 220000)

  } else if (crime == "liquor laws" | crime == 22){

    subsetData <- dplyr::filter(.data, !!var >= 220000 & !!var < 230000)

  } else if (crime == "drunkenness" | crime == 23){

    subsetData <- dplyr::filter(.data, !!var >= 230000 & !!var < 240000)

  } else if (crime == "disorderly conduct" | crime == 24){

    subsetData <- dplyr::filter(.data, !!var >= 240000 & !!var < 250000)

  } else if (crime == "vagrancy" | crime == 25){

    subsetData <- dplyr::filter(.data, !!var >= 250000 & !!var < 260000)

  } else if (crime == "all other offenses" | crime == 26){

    subsetData <- dplyr::filter(.data, !!var >= 260000 & !!var < 270000)

  } else if (crime == "suspicion" | crime == 27){

    subsetData <- dplyr::filter(.data, !!var >= 270000 & !!var < 280000)

  } else if (crime == "curfew and loitering laws-persons under 18" | crime == 28){

    subsetData <- dplyr::filter(.data, !!var >= 280000 & !!var < 290000)

  } else if (crime == "runaways-persons under 18" | crime == 29){

    subsetData <- dplyr::filter(.data, !!var >= 290000)

  } else {

    stop("The given argument for crime does not match an acceptible input.")

  }

  # return output
  return(subsetData)

}

#' Remove Negative Counts
#'
#' @description Removes the row that contains -1 in a specified column
#'
#' @details Crimes with a count of -1 are crimes that were determined to be unfounded
#'
#' @usage cs_filter_count(.data, var)
#'
#' @param .data A tbl
#' @param var the name of the column
#'
#' @return returns the data frame with the rows containing -1 removed
#'
#' @examples
#' testData <- january2018
#' testData <- cs_filter_count(testData,Count)
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

  #Filters for counts of 1
  .data %>%
    dplyr::filter(((!!var)) == 1)
}
