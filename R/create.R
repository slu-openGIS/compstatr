#' Prepare Raw Data
#'
#' @description Data downloaded from the St. Louis Metropolitan Police Department
#'    are downloaded with incorrect file paths - e.g. \code{January2008.CSV.html}.
#'    This function iterates over all files in a given path and replaces their
#'    file extensions. Thus \code{January2008.CSV.html} will be replaced by
#'    \code{january2008.csv}. There should be no more than 12 files in a given path,
#'    and all should correspond to the same year.
#'
#' @usage cs_prep_year(path, verbose = FALSE)
#'
#' @param path File path where raw STLMPD data are
#' @param verbose If \code{TRUE}, returns a tibble with results; otherwise if \code{FALSE}, no
#'     output is returned.
#'
#' @return A tibble containing old file names and new file names for reference is \code{verbose = TRUE}.
#'     Otherwise, no output is returned. This function will change all problematic filenames in the
#'     specified path.
#'
#' @examples
#' # create temporary directory
#' tmpdir <- tempdir()
#' fs::dir_create(paste0(tmpdir,"/data/"))
#'
#' # load sample files into temporary directory
#' cs_example(path = paste0(tmpdir,"/data/"))
#'
#' # list files
#' list.files(paste0(tmpdir,"/data/"))
#'
#' # prep sample files
#' cs_prep_year(path = paste0(tmpdir,"/data/"))
#'
#' # list files again
#' list.files(paste0(tmpdir,"/data/"))
#'
#' # delete data
#' fs::dir_delete(paste0(tmpdir,"/data/"))
#'
#' # create temporary directory
#' fs::dir_create(paste0(tmpdir,"/data/"))
#'
#' # load sample files into temporary directory
#' cs_example(path = paste0(tmpdir,"/data/"))
#'
#' # prep sample files
#' cs_prep_year(path = paste0(tmpdir,"/data/"), verbose = TRUE)
#'
#' # delete data again
#' fs::dir_delete(paste0(tmpdir,"/data/"))
#'
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#' @importFrom fs file_move
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#'
#' @export
cs_prep_year <- function(path, verbose = FALSE){

  # check parameters
  if (is.logical(verbose) == FALSE){
    stop("The 'verbose' parameter only accepts 'TRUE' or 'FALSE' as valid arguments.")
  }

  # create vector of filenames
  files <- list.files(path)

  # check number of files
  if (length(files) > 12){

    stop('There are too many files in the specified folder. Edit crime files in yearly batches of 12 monthly files.')

  } else if (length(files) < 12){

    warning('There are fewer than 12 files in the specified folder. You are only editing a partial year.')

  }

  # detect html in file extensions
  html <- stringr::str_detect(files, pattern = ".html$")

  # create data frame of files and file extensions
  data <- data.frame(files = files, html = html, stringsAsFactors = FALSE)

  # subset data frame and convert to vector
  data <- dplyr::filter(data, html == TRUE)
  problemFiles <- as.vector(data$files)

  # iterate and produce optional output
  if (verbose == TRUE){

    # iterate over each filename, renaming it and coverting to lowercase
    problemFiles %>%
      split(problemFiles) %>%
      purrr::map_chr(~ cs_edit_filename(path = path, file = .x)) -> changes

    # create vector of new filenames
    orignal <- names(changes)
    names(changes) <- NULL

    # create output
    out <- dplyr::as_tibble(data.frame(
      original = orignal,
      new = changes,
      stringsAsFactors = FALSE
    ))

    # return output
    return(out)

  } else if (verbose == FALSE){

    # iterate over each filename, renaming it and coverting to lowercase
    problemFiles %>%
      split(problemFiles) %>%
      purrr::map(~ cs_edit_filename(path = path, file = .x)) -> out

  }

}

# edit an individual file name
cs_edit_filename <- function(path, file){

  # construct a new file name that is all lower case, removes .html from end
  newFile <- tolower(stringr::str_replace(file, pattern = ".html$", replacement = ""))

  # create new file paths, adding a forward slash between path and filename if necessary
  if (stringr::str_detect(path, pattern = "/$") == FALSE){

    filePath <- stringr::str_c(path, "/", file)
    newPath <- stringr::str_c(path, "/", newFile)

  } else {

    filePath <- stringr::str_c(path, file)
    newPath <- stringr::str_c(path, newFile)

  }

  # rename file
  fs::file_move(filePath, newPath)

  # return output
  return(newFile)

}

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
#' # create temporary directory
#' tmpdir <- tempdir()
#' fs::dir_create(paste0(tmpdir,"/data/"))
#'
#' # load sample files into temporary directory
#' cs_example(path = paste0(tmpdir,"/data/"))
#'
#' # prep sample files
#' cs_prep_year(path = paste0(tmpdir,"/data/"))
#'
#' # load sample files
#' yearList17 <- cs_load_year(path = paste0(tmpdir,"/data/"))
#'
#' # delete data
#' fs::dir_delete(paste0(tmpdir,"/data/"))
#'
#' # print year-list object
#' yearList17
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

  # return output
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

  # return output
  return(name)

}
