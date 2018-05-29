#' Seperate Date Occur
#'
#' @description Creates two columns. One contains month, day, and year and the other contains hour, and minute.
#'
#' @usage cs_parse_date(.data, var, newDate, newTime)
#'
#' @param .data a data frame
#' @param var a column containing month, day, year, and time seperated by "/"
#' @param newDate column containing month, day, and year
#' @param newTime column containing hour and minute
#'
#' @return
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom tidyr select
#' @importFrom dplyr separate
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#'@export
cs_parse_date <- function(.data,var, newDate,newTime){#Seperates DateOccur into four columns and removes input column

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be separated must be specified for .data')
  }

  if (missing(var)) {
    stop('The column containing the data to be separated must be specified for variable')
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$var)) {
    var <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    var <- rlang::quo(!! rlang::sym(var))
  }

  if (!is.character(paramList$newDate)) {
    newDate <- rlang::enquo(newDate)
  } else if (is.character(paramList$newDate)) {
    newDate <- rlang::quo(!! rlang::sym(newDate))
  }

  if (!is.character(paramList$newTime)) {
    newTime <- rlang::enquo(newTime)
  } else if (is.character(paramList$newTime)) {
    newTime <- rlang::quo(!! rlang::sym(newTime))
  }

  .data %>%
    dplyr::mutate(newVar = mdy_hm((!!var))) %>%
    tidyr::separate((!!var), c((!!newDate),(!!newTime)), sep = " ")


}

