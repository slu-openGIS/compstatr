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
#'   \item{complaint}{complaint record number}
#'   \item{coded_month}{year and month crime reported}
#'   \item{date_occur}{date and time of crime}
#'   \item{flag_crime}{Returns a Y when a crime occurred}
#'   \item{flag_unfounded}{Reported crime was investigated and determined to be unfounded}
#'   \item{flag_administrative}{Reported crime had a change in classification}
#'   \item{count}{Returns a 1 or -1 for counting purposes}
#'   \item{flag_cleanup}{Returns information if administrative cleanup occured}
#'   \item{crime}{Uniform Crime Reporting code}
#'   \item{district}{Number corresponding to the police determined district}
#'   \item{description}{Name of the crime}
#'   \item{ileads_address}{I/Leads system address}
#'   \item{ileads_street}{I/Leads system street}
#'   \item{neighborhood}{Number corresponding to a neighborhood}
#'   \item{location_name}{Common “Location Name” (i.e. Zoo, Scottrade Center, etc.)}
#'   \item{location_comment}{Information to provide context to the location (i.e. Alley, Restaraunt Name)}
#'   \item{cad_address}{The Computer-Aided Dispatch address is the reported address by the 911 caller}
#'   \item{cad_street}{The Computer-Aided Dispatch street is the reported street by the 911 caller}
#'   \item{x_coord}{X-coordinates in the NAD83 format}
#'   \item{y_coord}{Y-coordinates in the NAD83 format}
#'   }
#'
#' @source \href{http://www.slmpd.org}{St. Louis Metropolitan Police Department}
#'
#' @examples
#' str(january2018)
#' head(january2018)
#'
"january2018"

