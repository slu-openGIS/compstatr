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
#' @importFrom dplyr filter
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

  } else if (crime == "part 1" | crime == "Part 1"){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var <= 90000, TRUE, FALSE))

  } else if (crime == "Homicide" | crime == "homicide" | crime == "Murder" | crime == "murder" | crime == 1){

    cleanData <- dplyr::mutate(.data, !!newVarN := ifelse(!!var >= 10000 & !!var < 20000, TRUE, FALSE))

  }

  return(cleanData)

}
