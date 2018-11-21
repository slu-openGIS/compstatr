#' Project Data
#'
#' @description \code{cs_project} converts STLMPD or St. Louis County data into a simple
#'     features object. For STLMPD data, this is done using the \code{XCoord} and
#'     \code{YCoord} columns. For St. Louis county data, this is done using the
#'     \code{X} and \code{Y} columns.
#'
#' @details Simple features objects are a means for storing and representing spatial data
#'     in \code{R}. It is implemented using the package \code{\link{sf}}, which is not a
#'     package that is automatically installed when you install \code{compstatr} since it
#'     has some complicated dependencies. To use this function, first install the
#'     \code{\link{sf}} package and its dependencies (see
#'     \url{https://github.com/r-spatial/sf}).
#'
#' @usage cs_project(.data, varX, varY, crs)
#'
#' @param .data A tibble with raw CSB data
#' @param area One of either "city" or "county"
#' @param varX Name of column containing x coordinate data
#' @param varY Name of column containing y coordinate data
#'
#' @export
cs_project <- function(.data, area, varX, varY, crs){

  # this function relies on sf to be present, but given dependencies it is not
  # a required part of the package, only suggested
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package \"sf\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

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
