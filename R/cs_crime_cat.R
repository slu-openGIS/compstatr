#' Categorize Crime
#'
#' @description The SLMPD data contains 5 or 6 digit codes to refer to
#'     specific categories of crime. \code{cs_crime_cat} transforms these
#'     into either string, factor, or simplified numeric categories
#'     like "murder" or "aggrevated assault".
#'
#' @details The categories used here are derived from the U.S. Federal
#'     Bureau of Investigation's Uniform Crime Reporting codes.
#'
#' @param .data A tbl
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param newVar Name of output variable to be created with simplified categories
#' @param output Type of output - either string, factor, or numeric
#'
#' @return A copy of the data frame with the new output variable appended to it.
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
cs_crime_cat <- function(.data, var, newVar, output = c("string", "factor", "numeric")){

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

  if (output == "string" | output == "factor"){

    cleanData <- .data %>%
      dplyr::mutate(!!newVarN := case_when(
        !!var >= 10000 & !!var < 20000 ~ "Homicide",
        !!var >= 20000 & !!var < 30000 ~ "Forcible Rape",
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

      cleanData <- dplyr::mutate(cleanData, !!newVarN := as.factor(!!varN))

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

  return(cleanData)

}
