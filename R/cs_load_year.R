#' Create Year List Object
#'
#' @description \code{cs_load_year} is used to load a set of \code{.csv} files
#'    contained in the given directory. This should be used to load a full
#'    year worth of data or a partial year. There should be no more than 12
#'    files in a given path, and all should correspond to the same year. All
#'    columns will be read in as character data in order to address inconsistencies
#'    in how the data are created. When \link{cs_collapse} is executed, variables
#'    will be converted numeric when doing so is applicable.
#'
#' @usage cs_load_year(path)
#'
#' @param path A file path
#'
#' @return A year-list object containing 12 tibbles - one per month - worth
#'    of crime data stored within a list.
#'
#' @examples
#' \dontrun{
#' yearList08 <- cs_load_year(path = "data/raw/2008")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom readr cols
#' @importFrom readr col_character
#' @importFrom readr read_csv
#' @importFrom stringr str_sub
#'
#' @export
cs_load_year <- function(path){

  # create list of all files at path that are csv
  files <- dir(path = path, pattern = "*.csv")

  # check number of files
  if (length(files) > 12){

    stop('There are too many files in the specified folder. Load crime files in yearly batches of 12 monthly files.')

  } else if (length(files) < 12){

    warning('There are fewer than 12 files in the specified folder. You are only loading a partial year.')

  }

  # read csv files into year list objects
  files %>%
    purrr::map(~ suppressMessages(suppressWarnings(readr::read_csv(file.path(path, .), col_types = readr::cols(.default = readr::col_character()))))) -> out

  # create list of months associated with year list object items
  out %>%
    purrr::map(cs_identifyMonth) -> nameList

  # convert list of months to vector
  nameVector <- unlist(nameList, recursive = TRUE, use.names = TRUE)

  # apply vector to data
  names(out) <- nameVector

  # add new class
  class(out) <- append(class(out), "cs_year_list")

  # return year list object
  return(out)

}

# Extract Month of a Given Year List Object Item
#
# @description  This uses the value of the first observation's coded month as the basis for
#     identifying which month the data are from.
#
# @param .data A year list object name
# @param read A logical scalar; if \code{TRUE}, return output structured for initial reading of
#    data into R. If \code{FALSE}, use for data validation.
#
cs_identifyMonth <- function(.data, read = TRUE){

  # depending on number of columns, the CodedMonth variable is named differently
  # the if elseif statements pull the first value from CodedMonth

  if (length(.data) == 18){

    monthVal <- .data$MonthReportedtoMSHP[1]

  } else if (length(.data) == 20){

    monthVal <- .data$CodedMonth[1]

  } else if (length(.data) == 26){

    monthVal <- .data$`Coded Month`[1]

  }

  # extract the last two digits from the coded month value
  monthString <- stringr::str_sub(monthVal, start = -2)

  if (read == TRUE){

    # convert those last two digits into a string month name
    out <- cs_matchMonth(monthString)

  } else if (read == FALSE){

    out <- as.numeric(monthString)

  }

  return(out)

}

# Match Extract Month with Month Name
#
# @description Based on the result of cs_identifyMonth, this function returns the
#     appropriate string name.
#
# @param x The last two characters of the first observation's coded month value
#
cs_matchMonth <- function(x){

  # the last two digits from two digits from the coded month value are passed to this function as x
  # depending on the value, the correct month string name is returned

  if (x == "01") {

    name <- "January"

  } else if (x == "02") {

    name <- "February"

  } else if (x == "03") {

    name <- "March"

  } else if (x == "04") {

    name <- "April"

  } else if (x == "05") {

    name <- "May"

  } else if (x == "06") {

    name <- "June"

  } else if (x == "07") {

    name <- "July"

  } else if (x == "08") {

    name <- "August"

  } else if (x == "09") {

    name <- "September"

  } else if (x == "10") {

    name <- "October"

  } else if (x == "11") {

    name <- "November"

  } else if (x == "12") {

    name <- "December"

  }

  return(name)

}
