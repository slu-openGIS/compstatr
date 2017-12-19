#'' Remove Negative Counts
#'Removes values of -1 from a column
#'@param .data a data frame
#'@param variable the name of the column
#'@return returns the data frame with the rows containing -1 removed
#'@importFrom dplyr %>%
#'@importFrom dplyr filter
#'@importFrom rlang quo
#'@importFrom rlang enquo
#'@importFrom rlang quo_name
#'@export
removeNegativeCount <- function(.data,variable){#Removes count equal to -1
  # save parameters to list
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be seperated must be specified for .data')
  }

  if (missing(variable)) {
    stop('The column containing the data to be separated must be specified for variable')
  }

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(variable))
  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }


  .data %>%
    dplyr::filter(((!!var)) == 1)
}
