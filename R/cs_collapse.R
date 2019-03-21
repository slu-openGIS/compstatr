#' Combine Months in Year List Object
#'
#' @description This function takes a year-list containing individual tibbles - one per
#'    month - that have been validated and collapses them into a single tibble.
#'
#' @details \code{cs_collapse} applies common sense variable classes to a number
#'    of variables. This is motivated by issues that originate with SLMPD \code{.csv}
#'    files. When they are imported, the \pkg{readr} package sometimes applies
#'    the incorrect variable classes because of formatting issues in the tables.
#'    Since the tables have inconsistent variable names and numbers of variables,
#'    all variables are imported as \code{chr} data. During \code{cs_collapse}'s
#'    exectuion, the following changes are made:
#'
#' \describe{
#'   \item{Count}{Converted to \code{int}}
#'   \item{Crime}{Converted to \code{int}}
#'   \item{District}{Converted to \code{int}}
#'   \item{ILEADSAddress}{Converted to \code{int}}
#'   \item{Neighborhood}{Converted to \code{int}}
#'   \item{CADAddress}{Converted to \code{int}}
#'   \item{XCoord}{Converted to \code{dbl}}
#'   \item{YCoord}{Converted to \code{dbl}}
#' }
#'
#' @usage cs_collapse(.data)
#'
#' @param .data A list containing monthly crime data
#'
#' @return A tibble containing all crime data in a given year-list object.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom purrr map
#'
#' @export
cs_collapse <- function(.data){

  # undefined global variables
  CodedMonth = DateOccur = NULL

  # check for missing parameters
  if (missing(.data)) {
    stop('A existing data frame with data to be separated must be specified for .data')
  }

  # extract each month, collapse, and re-order
  purrr::map(.data, dplyr::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(CodedMonth, DateOccur) -> out

  # reformat variables
  out %>%
    dplyr::mutate(
        Count = as.integer(Count),
        Crime = as.integer(Crime),
        District = as.integer(District),
        ILEADSAddress = as.integer(ILEADSAddress),
        Neighborhood = as.integer(Neighborhood),
        CADAddress = as.integer(CADAddress),
        XCoord = as.numeric(XCoord),
        YCoord = as.numeric(YCoord)
      ) -> out

  # return combined object
  return(out)

}
