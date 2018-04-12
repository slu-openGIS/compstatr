#' Crimes in St. Louis, January 2018
#'
#' A data set containing all reported crimes in St. Louis, Missouri during January 2018.
#'
#' @docType data
#'
#' @usage data(january2018)
#'
#' @format A tibble with 3825 rows and 20 variables:
#' \describe{
#'   \item{Complaint}{complaint record number}
#'   \item{CodedMonth}{year and month crime reported}
#'   \item{DateOccur}{date and time of crime}
#'   \item{FlagCrime}{}
#'   \item{FlagUnfounded}{}
#'   \item{FlagAdministrative}{}
#'   \item{FlagCount}{}
#'   \item{FlagCleanup}{}
#'   \item{Crime}{}
#'   \item{District}{}
#'   \item{Description}{}
#'   \item{ILEADSAddress}{}
#'   \item{ILEADSStreet}{}
#'   \item{Neighborhood}{}
#'   \item{LocationName}{}
#'   \item{LocationComment}{}
#'   \item{CADAddress}{}
#'   \item{CADStreet}{}
#'   \item{XCoord}{}
#'   \item{YCoord}{}
#'   }
#'
#' @source \href{http://www.slmpd.org}{St. Louis Metropolitan Police Department}
#'
#' @examples
#' str(january2018)
#' head(january2018)
#'
"january2018"

