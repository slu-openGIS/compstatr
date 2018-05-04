#' Seperate Date Occur
#'
#' @description Seperates a column containing month, day, year, and time into 4 different
#'     columns and removes the input column.
#'
#' @usage cs_parse_date(.data, var)
#'
#' @param .data a data frame
#' @param var a column containing month, day, year, and time seperated by "/"
#' @param newVar
#' @param newVar2
#' @param newVar3
#'
#' @return
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr separate
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#'@export
cs_parse_date <- function(.data,var, newVar, newVar2, newVar3){#Seperates DateOccur into four columns and removes input column

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
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

  .data %>%
    mutate(newVar = mdy_hm((!!var))) %>%
    separate(DateOccur, c("newVar2","newVar3"), sep = " ")

}
