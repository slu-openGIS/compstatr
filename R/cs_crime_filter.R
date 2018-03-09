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

  if (crime == "violent"){

    subsetData <- dplyr::filter(.data, !!var <= 50000)

  } else if (crime == "part 1" | crime == "Part 1"){

    subsetData <- dplyr::filter(.data, !!var <= 90000)

  } else if (crime == "Homicide" | crime == "homicide" | crime == "Murder" | crime == "murder" | crime == 1){

    subsetData <- dplyr::filter(.data, !!var >= 10000 & !!var < 20000)

  } else if (crime == "Arson" | crime == "arson" | crime == 8){

    subsetData <- dplyr::filter(.data, !!var >= 80000 & !!var < 90000)

  }

  return(subsetData)

}
