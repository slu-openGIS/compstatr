#' Identify Crimes
#'
#' @description \code{cs_crime} can be used to easily identify
#'     crimes based on a specific single UCR categories or common groupings.
#'
#' @usage cs_crime(.data, var, newVar, crime)
#'
#' @param .data A tbl
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param newVar Name of output variable to be created with logical data
#' @param crime A string describing the crime type to be identified
#'
#' @return A tibble with a logical vector that is \code{TRUE} if the given crime matches
#'     the category given in the function.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang sym
#'
#' @export
cs_crime <- function(.data, var, newVar, crime){

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  newVarN <- rlang::quo_name(rlang::enquo(newVar))

  if (crime == "violent"){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var <= 50000, TRUE, FALSE))

  } else if (crime == "property" | crime == "Property")

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 50000 & !!var < 90000, TRUE, FALSE))

  } else if (crime == "part 1" | crime == "Part 1"){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var <= 90000, TRUE, FALSE))

  } else if (crime == "Homicide" | crime == "homicide" | crime == "Murder" | crime == "murder" | crime == 1){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 10000 & !!var < 20000, TRUE, FALSE))

  } else if (crime == "Forcible Rape" | crime == "Forcible Rape" | crime == "Rape" | crime == "rape" | crime == 2){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 20000 & !!var < 30000, TRUE, FALSE))

} else if (crime == "robbery" | crime == "Robbery" | crime == 3){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 30000  & !!var < 40000, TRUE, FALSE))

} else if (crime == "aggravated Assault" | crime == "Aggravated Assault" | crime == 4){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 40000 & !!var < 50000, TRUE, FALSE))

} else if (crime == "burglary" | crime == "Burglary" | crime == 5){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 50000 & !!var < 60000, TRUE, FALSE))

} else if (crime == "larceny-theft" | crime == "Larceny-theft" | crime == 6){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 60000  & !!var < 70000, TRUE, FALSE))

} else if (crime == "motor vehicle theft" | crime == "Motor Vehicle Theft" | crime == 7){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 70000 & !!var < 80000, TRUE, FALSE))

} else if (crime == "Arson" | crime == "arson" | crime == 8){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 80000 & !!var < 90000, TRUE, FALSE))

} else if (crime == "part 2" | crime == "Part 2" | crime == "part II" | crime == "Part II"){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 90000, TRUE, FALSE))

} else if (crime == "Other Assaults" | crime == "other assaults" | crime == 9){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 90000 & !!var < 100000, TRUE, FALSE))

} else if (crime == "Forgery and Counterfeiting" | crime == "forgery and counterfeiting" | crime == 10){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 100000 & !!var < 110000, TRUE, FALSE))

} else if (crime == "Fraud" | crime == "fraud" | crime == 11){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 110000 & !!var < 120000, TRUE, FALSE))

} else if (crime == "Embezzlement" | crime == "embezzlement" | crime == 12){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 120000 & !!var < 130000, TRUE, FALSE))

} else if (crime == "stolen property" | crime == "Stolen Property" | crime == 13){
  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 130000 & !!var < 140000, TRUE, FALSE))

} else if (crime == "Vandalism" | crime == "vandalism" | crime == 14){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 140000 & !!var < 150000, TRUE, FALSE))

} else if (crime == "weapons" | crime == "Weapons" | crime == 15){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 150000 & !!var < 160000, TRUE, FALSE))

} else if (crime == "prostitution and commercialized vice" | crime == "Prostitution and Commercialized Vice" | crime == 16){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 160000 & !!var < 170000, TRUE, FALSE))

} else if (crime == "sex offenses" | crime == "Sex Offense" | crime == 17){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 170000 & !!var < 180000, TRUE, FALSE))

} else if (crime == "drug abuse violations" | crime == "Drug Abuse Violations" | crime == 18){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 180000 & !!var < 190000, TRUE, FALSE))

} else if (crime == "Gambling" | crime == "gambling" | crime == 19){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 190000 & !!var < 200000, TRUE, FALSE))

} else if (crime == "offenses against the family and children" | crime == "Offense Against the Family and Children" | crime == 20){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 200000 & !!var < 210000, TRUE, FALSE))

} else if (crime == "driving under the influence" | crime == "Driving Under the Influence" | crime == 21){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 210000 & !!var < 220000, TRUE, FALSE))

} else if (crime == "liquor laws" | crime == "Liquor Laws" | crime == 22){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 220000 & !!var < 230000, TRUE, FALSE))

} else if (crime == "drunkenness" | crime == "Drunkenness" | crime == 23){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 230000 & !!var < 240000, TRUE, FALSE))

} else if (crime == "disorderly conduct" | crime == "Disorderly Conduct" | crime == 24){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 240000 & !!var < 250000, TRUE, FALSE))

} else if (crime == "vagrancy" | crime == "Vagrancy" | crime == 25){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 250000 & !!var < 260000, TRUE, FALSE))

} else if (crime == "all other offenses" | crime == "All Other Offenses" | crime == 26){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 260000 & !!var < 270000, TRUE, FALSE))

} else if (crime == "suspicion" | crime == "Suspicion" | crime == 27){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 270000 & !!var < 280000, TRUE, FALSE))

} else if (crime == "curfew and loitering laws" | crime == "Curfew and Loitering Laws" | crime == 28){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 280000 & !!var < 290000, TRUE, FALSE))

} else if (crime == "runaways" | crime == "Runaways" | crime == 29){

  cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 290000 & !!var < 300000, TRUE, FALSE))

}

  return(cleanData)

}
