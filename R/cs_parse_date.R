#' Seperate Date Occur
#'
#' @description Seperates a column containing month, day, year, and time into 4 different
#'     columns and removes the input column.
#'
#' @param .data a data frame
#' @param var a column containing month, day, year, and time seperated by "/"
#'
#' @return returns 4 different columns containing, month, day, year, and time
#'
#' @importFrom dplyr %>%
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
    tidyr::separate((!!var), c("monthOccur","dayOccur","yearOccur","timeOccur"), "/|\\ ", remove = TRUE)

}
