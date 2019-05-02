#' Create a Single Address Field
#'
#' @description The street address data in SLMPD releases (either \code{ILEADSAddress} and
#'     \code{ILEADSStreet} or \code{CADAddress} and \code{CADStreet}) are stored in separate
#'     columns. In order to faciliate geocoding, this function combines the fields and
#'     removes inappropriate characters in the address fields.
#'
#' @usage cs_address(.data, address, street, newVar)
#'
#' @param .data A tibble or data frame
#' @param address Name of address number variable (typically either \code{ILEADSAddress}
#'     or \code{CADAddress})
#' @param street Name of street name variable (typically either \code{ILEADSStreet}
#'     or \code{CADStreet})
#' @param newVar Name of new variable to store concatenated address
#'
#' @return A copy of the object with a character vector that contains the concatenated
#'     street address data.
#'
#' @examples
#' # load example data
#' testData <- january2018
#'
#' # add concatenated address variable
#' testData <- cs_address(testData, address = ILEADSAddress, street = ILEADSStreet, newVar = address)
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang quo
#' @importFrom rlang enquo
#' @importFrom rlang sym
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#'
#' @export
cs_address <- function(.data, address, street, newVar){

  # check for missing parameters
  if (missing(.data)) {
    stop("An existing data frame with crime data must be specified for '.data'.")
  }

  if (missing(address)) {
    stop("The column containing address numbers must be specified for 'address'.")
  }

  if (missing(street)) {
    stop("The column containing street names must be specified for 'street'.")
  }

  if (missing(newVar)) {
    stop("A new variable name must be specified for 'newVar'.")
  }

  #save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  if (!is.character(paramList$address)) {
    addressQ <- rlang::enquo(address)
  } else if (is.character(paramList$address)) {
    addressQ <- rlang::quo(!! rlang::sym(address))
  }

  if (!is.character(paramList$street)) {
    streetQ <- rlang::enquo(street)
  } else if (is.character(paramList$street)) {
    streetQ <- rlang::quo(!! rlang::sym(street))
  }

  if (!is.character(paramList$newVar)) {
    newVarQ <- rlang::enquo(newVar)
  } else if (is.character(paramList$newVar)) {
    newVarQ <- rlang::quo(!! rlang::sym(newVar))
  }

  newVarQN <- rlang::quo_name(rlang::enquo(newVar))

  # concatenate address
  .data <- dplyr::mutate(.data, !!newVarQN := stringr::str_c(!!addressQ, !!streetQ, sep = " "))

  # clean address
  .data <- dplyr::mutate(.data, !!newVarQN := ifelse(stringr::str_detect(string = !!newVarQ, pattern = "^[0\\b]") == TRUE,
                                   stringr::str_replace(string = !!newVarQ, pattern = "^[0\\b]", replacement = ""),
                                   address))
  .data <- dplyr::mutate(.data, !!newVarQN := stringr::str_trim(!!newVarQ))

  # return output
  return(.data)

}
