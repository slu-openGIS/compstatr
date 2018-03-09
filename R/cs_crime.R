#' Identify Violent Crimes
#'
#' @description Violent crimes, as defined by the U.S. Federal Bureau of
#'     Investigation, are murder, rape, aggrevated assault, and burglary.
#'     \code{cs_crime_violent} can be used to identify specific single
#'     UCR categories or common groupings for purposes of variable
#'     creation or subsettings.
#'
#' @usage cs_crime(.data, var, crime)
#'
#' @param .data A tbl
#' @param var Name of variable with 5 or 6 digit crime codes
#' @param crime A string describing the crime type to be identified
#'
#' @return A logical vector
#'
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#' @export
cs_crime <- function(.data, var, crime){

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  varN <- rlang::quo_name(rlang::enquo(var))

  if (crime == "violent"){

    output <- ifelse(.data[,varN] <= 50000, TRUE, FALSE)

  } else if (crime == "part 1" | crime == "Part 1"){

    output <- ifelse(.data[,varN] <= 90000, TRUE, FALSE)

  }

  return(output)

}
