#''Seperate Date Occur
#'Seperates a column containing month, day, year, and time into 4 different columns and removes the input column.
#'@param .data a data frame
#'@param variable a column containing month, day, year, and time seperated by "|"
#'@return returns 4 different columns containing, month, day, year, and time
#'@importFrom dplyr %>%
#'@importFrom tidyr separate
#'@importFrom rlang quo
#'@importFrom rlang enquo
#'@importFrom rlang quo_name

separateDateOccur <- function(.data,variable){#Seperates DateOccur into four columns and removes input column
  paramList <- as.list(match.call())
  varN <- rlang::quo_name(rlang::enquo(variable))
  if (!is.character(paramList$variable)) {
    var <- rlang::enquo(variable)
  } else if (is.character(paramList$variable)) {
    var <- rlang::quo(!! rlang::sym(variable))
  }
  .data %>%
    tidyr::separate((!!var), c("monthOccur","dayOccur","yearOccur","timeOccur"), "/|\\ ", remove = TRUE)

}
