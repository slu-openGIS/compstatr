#' Seperate Date Occur
#'
#' @description Creates a column containing hour month
#'
#' @usage cs_parse_date(.data, var)
#'
#' @param .data a data frame
#' @param var a column containing month, day, year, and time seperated by "/"
#' @param newVar column containing month, day, year, hour, and minute
#' @param newVar2 column containing month, day, and year
#' @param newVar3 column containing hour and minute
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
cs_parse_date <- function(.data,var){#Seperates DateOccur into four columns and removes input column

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
    separate(DateOccur, c("newVar2","newVar3"), sep = " ") %>%
    select(-newVar)


}
