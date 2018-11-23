#' Project Data
#'
#' @description \code{cs_project} converts STLMPD or St. Louis County data into a simple
#'     features object. For STLMPD data, this is done using the \code{XCoord} and
#'     \code{YCoord} columns. For St. Louis county data, this is done using the
#'     \code{X} and \code{Y} columns.
#'
#' @usage cs_project(.data, area, varX, varY, crs)
#'
#' @param .data A tibble with crime data
#' @param area One of either "city" or "county"
#' @param varX Name of column containing x coordinate data
#' @param varY Name of column containing y coordinate data
#' @param crs integer with the EPSG code, or character with proj4string representing the
#'     coordinate reference system
#'
#' @export
cs_project <- function(.data, area, varX, varY, crs){

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
  if (area == "city"){

    # project data to NAD 1983 StatePlane (feet)
    out <- sf::st_as_sf(.data, coords = c(x = varXN, y = varYN), crs = 102696)

  } else if (area == "county"){

    # project data to NAD 1983
    out <- sf::st_as_sf(.data, coords = c(x = varXN, y = varYN), crs = 4269)

  }

  # optionally reproject data
  if(!is.null(crs)){

      out <- sf::st_transform(out, crs = crs)

  }

  # return output
  return(out)

}
