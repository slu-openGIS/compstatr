#' Replace Coordinates with 0 Value with NA
#'
#' @description This function a specified column from the data frame and
#'     replaces cells that have the value 0 with NA
#'
#' @usage cs_replace0(.data, var)
#'
#' @param .data A tibble or data frame
#' @param var Name of column containing coordinate data
#'
#' @return A tibble or data frame with the coordinate column updated.
#'
#' @examples
#' # load example data
#' testData <- january2018
#'
#' # replace 0s in the x and y coordinate variables
#' testData <- cs_replace0(testData, var = XCoord)
#' testData <- cs_replace0(testData, var = YCoord)
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
cs_replace0 <- function(.data, var){

  # save parameters to lis
  paramList <- as.list(match.call())

  # check for missing parameters
  if (missing(.data)) {
    stop("An existing data frame with data to be edited must be specified for '.data'.")
  }

  if (missing(var)) {
    stop("The column containing coordinate data must be specified for 'var'.")
  }

  #quote input variables
  varN <- rlang::quo_name(rlang::enquo(var))

  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # update coordinate col
  .data <- dplyr::mutate(.data, !!varN := ifelse((!!varQ) == 0, NA, (!!varQ)))

  # return output
  return(.data)

}

#' Identify Missing Coordinates
#'
#' @description \code{cs_missingXY} compares X and Y coordinates and adds a
#'     logical column that identifies observations that are missing coordinate data.
#'
#' @usage cs_missingXY(.data, varX, varY, newVar)
#'
#' @param .data A tibble or data frame
#' @param varX Name of column containing x coordinate data
#' @param varY Name of column containing y coordinate data
#' @param newVar Name of new column that is \code{TRUE} if coordinate data are
#'     missing and \code{FALSE} otherwise.
#'
#' @return A tibble or data frame with a logical vector appended to it.
#'
#' @examples
#' # load example data
#' testData <- january2018
#'
#' # identify missing x and y coordinates
#' testData <- cs_missingXY(testData, varX = XCoord, varY = YCoord, newVar = missingXY)
#'
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
cs_missingXY <- function(.data, varX, varY, newVar){

  # check for missing parameters
  if (missing(.data)) {
    stop("An existing data frame with data to be analyzed must be specified for '.data'.")
  }

  if (missing(varX)) {
    stop("The column containing the x coordinate must be specified for 'varX'.")
  }

  if (missing(varY)) {
    stop("The column containing the y coordinate must be specified for 'varY'.")
  }
  if (missing(newVar)) {
    stop("The output column name must be specified for 'newVar'.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  #quote input variables
  newVar <- rlang::quo_name(rlang::enquo(newVar))

  if (!is.character(paramList$varX)) {
    varxQ <- rlang::enquo(varX)
  } else if (is.character(paramList$varX)) {
    varxQ <- rlang::quo(!! rlang::sym(varX))
  }

  if (!is.character(paramList$varY)) {
    varyQ <- rlang::enquo(varY)
  } else if (is.character(paramList$varY)) {
    varyQ <- rlang::quo(!! rlang::sym(varY))
  }


  #create logical vector that is appended to data frame
  .data <- dplyr::mutate(.data, (!!newVar) := (ifelse((!!varxQ) == 0 & (!!varyQ) == 0 | is.na(!!varxQ) == TRUE & is.na(!!varyQ) == TRUE ,"TRUE","FALSE")))

  # return output
  return(.data)

}

#' Project Data
#'
#' @description \code{cs_projectXY} converts STLMPD data into a simple
#'     features object using the \code{XCoord} and \code{YCoord} columns.
#'
#' @usage cs_projectXY(.data, varX, varY, crs)
#'
#' @param .data A tibble or data frame
#' @param varX Name of column containing x coordinate data
#' @param varY Name of column containing y coordinate data
#' @param crs integer with the EPSG code, or character with proj4string representing the
#'     coordinate reference system
#'
#' @return A \code{sf} object with the crime data projected for mapping.
#'
#' # load example data
#' testData <- january2018
#'
#' # identify missing x and y coordinates
#' testData <- cs_missingXY(testData, varX = XCoord, varY = YCoord, newVar = missingXY)
#'
#' # subset to remove missing data
#' testData <- dplyr::filter(testData, missingXY == FALSE)
#'
#' # project data
#' testData_sf <- cs_projectXY(testData, varX = XCoord, varY = YCoord)
#'
#' # project data and transform to new CRS
#' testData_sf <- cs_projectXY(testData, varX = XCoord, varY = YCoord, crs = 4269)
#'
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#'
#' @export
cs_projectXY <- function(.data, varX, varY, crs){

  ## check for missing parameters
  if (missing(.data)) {
    stop("An existing data frame with data to be projected must be specified for '.data'.")
  }

  if (missing(varX)) {
    stop("The column containing the x coordinate must be specified for 'varX'.")
  }

  if (missing(varY)) {
    stop("The column containing the y coordinate must be specified for 'varY'.")
  }

  ## quote inputs
  varXN <- rlang::quo_name(rlang::enquo(varX))
  varYN <- rlang::quo_name(rlang::enquo(varY))

  # city data
  # project data to NAD 1983 StatePlane (feet)
  out <- sf::st_as_sf(.data, coords = c(x = varXN, y = varYN),
                      crs = "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-90.5 +k=0.9999333333333333 +x_0=250000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs")

  # optionally reproject data
  if(missing(crs) == FALSE){

    out <- sf::st_transform(out, crs = crs)

  }

  # return output
  return(out)

}
