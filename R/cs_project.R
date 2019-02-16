#' Project Data
#'
#' @description \code{cs_project} converts STLMPD data into a simple
#'     features object using the \code{XCoord} and \code{YCoord} columns.
#'
#' @usage cs_project(.data, varX, varY, crs)
#'
#' @param .data A tibble with crime data
#' @param varX Name of column containing x coordinate data
#' @param varY Name of column containing y coordinate data
#' @param crs integer with the EPSG code, or character with proj4string representing the
#'     coordinate reference system
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#'
#' @export
cs_project <- function(.data, varX, varY, crs){

  ## check for missing parameters
  if (missing(.data)) {
    stop('Please provide an argument for .data')
  }

  if (missing(varX)) {
    stop('Please provide an argument for varX, the x coordinate for your data.')
  }

  if (missing(varY)) {
    stop('Please provide an argument for varY, the y coordinate for your data.')
  }

  ## quote inputs
  varXN <- rlang::quo_name(rlang::enquo(varX))
  varYN <- rlang::quo_name(rlang::enquo(varY))

  # city data
  # project data to NAD 1983 StatePlane (feet)
  out <- sf::st_as_sf(.data, coords = c(x = varXN, y = varYN), crs = 102696)

  # optionally reproject data
  if(!is.null(crs)){

      out <- sf::st_transform(out, crs = crs)

  }

  # return output
  return(out)

}
