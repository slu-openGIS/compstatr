#' Filter Crimes
#'
#' @description \code{cs_crime_filter} can be used to subset based on
#'     specific single UCR categories or common groupings.
#'
#' @usage cs_crime_filter(.data, var, crime)
#'
#' @param .data A tbl
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param crime A string describing the crime type to be identified
#'
#' @return A subset tibble with only the specified crimes
#'
#' @importFrom dplyr filter
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang sym
#'
#' @export
cs_crime_filter <- function(.data, var, crime){

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  if (crime == "violent" | crime == "Violent"){

    subsetData <- dplyr::filter(.data, !!var <= 50000)

  } else if (crime == "property" | crime == "Property")

    subsetData <- dplyr::filter(.data, !!var >= 50000 & !!var < 90000)

  } else if (crime == "part 1" | crime == "Part 1" | crime == "part I" | crime == "Part I"){

    subsetData <- dplyr::filter(.data, !!var <= 90000)

  } else if (crime == "Homicide" | crime == "homicide" | crime == "Murder" | crime == "murder" | crime == 1){

    subsetData <- dplyr::filter(.data, !!var >= 10000 & !!var < 20000)

  } else if (crime == "Forcible Rape" | crime == "Forcible Rape" | crime == "Rape" | crime == "rape" | crime == 2){

    subsetData <- dplyr::filter(.data, !!var >= 20000 & !!var < 30000)

  } else if (crime == "robbery" | crime == "Robbery" | crime == 3){

    subsetData <- dplyr::filter(.data, !!var >= 30000 & !!var < 40000)

  } else if (crime == "aggravated assault" | crime == "Aggravated Assault" | crime == 4){

    subsetData <- dplyr::filter(.data, !!var >= 40000 & !!var < 50000)

  } else if (crime == "burglary" | crime == "Burglary" | crime == 5){

    subsetData <- dplyr::filter(.data, !!var >= 50000 & !!var < 60000)

  } else if (crime == "larceny-theft" | crime == "Larceny-theft" | crime == 6){

    subsetData <- dplyr::filter(.data, !!var >= 60000 & !!var < 70000)

  } else if (crime == "motor vehicle theft" | crime == "Motor Vehicle Theft" | crime == 7){

    subsetData <- dplyr::filter(.data, !!var >= 70000 & !!var < 80000)


  } else if (crime == "Arson" | crime == "arson" | crime == 8){

    subsetData <- dplyr::filter(.data, !!var >= 80000 & !!var < 90000)


  } else if (crime == "part 2" | crime == "Part 2" | crime == "part II" | crime == "Part II"){

    subsetData <- dplyr::filter(.data, !!var >= 90000)


  } else if (crime == "Other Assaults" | crime == "other assaults" | crime == 9){

    subsetData <- dplyr::filter(.data, !!var >= 90000 & !!var < 100000)


  } else if (crime == "Forgery and Counterfeiting" | crime == "forgery and counterfeiting" | crime == 10){

    subsetData <- dplyr::filter(.data, !!var >= 100000 & !!var < 110000)

  } else if (crime == "Fraud" | crime == "fraud" | crime == 11){

    subsetData <- dplyr::filter(.data, !!var >= 110000 & !!var < 120000)

  } else if (crime == "Embezzlement" | crime == "embezzlement" | crime == 12){

    subsetData <- dplyr::filter(.data, !!var >= 120000 & !!var < 130000)

  } else if (crime == "stolen property" | crime == "Stolen Property" | crime == 13){

    subsetData <- dplyr::filter(.data, !!var >= 130000 & !!var < 140000)

  } else if (crime == "Vandalism" | crime == "vandalism" | crime == 14){

    subsetData <- dplyr::filter(.data, !!var >= 140000 & !!var < 150000)

  } else if (crime == "weapons" | crime == "Weapons" | crime == 15){

    subsetData <- dplyr::filter(.data, !!var >= 150000 & !!var < 160000)

  } else if (crime == "prostitution and commercialized vice" | crime == "Prostitution and Commercialized Vice" | crime == 16){

    subsetData <- dplyr::filter(.data, !!var >= 160000 & !!var < 170000)

  } else if (crime == "sex offenses" | crime == "Sex Offense" | crime == 17){

    subsetData <- dplyr::filter(.data, !!var >= 170000 & !!var < 180000)

  } else if (crime == "drug abuse violations" | crime == "Drug Abuse Violations" | crime == 18){

    subsetData <- dplyr::filter(.data, !!var >= 180000 & !!var < 190000)

  } else if (crime == "Gambling" | crime == "gambling" | crime == 19){

    subsetData <- dplyr::filter(.data, !!var >= 190000 & !!var < 200000)

  } else if (crime == "offenses against the family and children" | crime == "Offense Against the Family and Children" | crime == 20){

    subsetData <- dplyr::filter(.data, !!var >= 200000 & !!var < 210000)

  } else if (crime == "driving under the influence" | crime == "Driving Under the Influence" | crime == 21){

    subsetData <- dplyr::filter(.data, !!var >= 210000 & !!var < 220000)

  } else if (crime == "liquor laws" | crime == "Liquor Laws" | crime == 22){

    subsetData <- dplyr::filter(.data, !!var >= 220000 & !!var < 230000)

  } else if (crime == "drunkenness" | crime == "Drunkenness" | crime == 23){

    subsetData <- dplyr::filter(.data, !!var >= 230000 & !!var < 240000)

  } else if (crime == "disorderly conduct" | crime == "Disorderly Conduct" | crime == 24){

    subsetData <- dplyr::filter(.data, !!var >= 240000 & !!var < 250000)

  } else if (crime == "vagrancy" | crime == "Vagrancy" | crime == 25){

    subsetData <- dplyr::filter(.data, !!var >= 250000 & !!var < 260000)

  } else if (crime == "all other offenses" | crime == "All Other Offenses" | crime == 26){

    subsetData <- dplyr::filter(.data, !!var >= 260000 & !!var < 270000)

  } else if (crime == "suspicion" | crime == "Suspicion" | crime == 27){

    subsetData <- dplyr::filter(.data, !!var >= 270000 & !!var < 280000)

  } else if (crime == "curfew and loitering laws-persons under 18" | crime == "Curfew and Loitering Laws-Persons under 18" | crime == 28){

    subsetData <- dplyr::filter(.data, !!var >= 280000 & !!var < 290000)

  } else if (crime == "runaways-persons under 18" | crime == "Runaways-Persons under 18" | crime == 29){

    subsetData <- dplyr::filter(.data, !!var >= 290000)



  }

  return(subsetData)

}
