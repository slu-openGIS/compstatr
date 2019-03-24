#' Load Example Files
#'
#' @description Adds a sample set of twelve files, one for each month of 2017,
#'     to the specified path. These are not full data files; each file has twenty
#'     observations. They can be used to practice preparing, loading, standardizing,
#'     and collapsing data.
#'
#' @param path File path where example data should be placed
#' @param overwrite Overwrite files if they exist. If this is FALSE and the file exists
#'     an error will be thrown.
#'
#' @examples
#' # create temporary directory
#' tmpdir <- tempdir()
#'
#' # load files into temporary directory
#' cs_example(path = tmpdir)
#'
#' # remove temporary directory
#' unlink(tmpdir, recursive = TRUE)
#'
#' @importFrom fs file_copy
#'
#' @export
cs_example <- function(path, overwrite = FALSE){

  # copy file, january
  fs::file_copy(system.file("extdata", "January2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/January2017.CSV.html"), overwrite = overwrite)

  # copy file, february
  fs::file_copy(system.file("extdata", "February2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/February2017.CSV.html"), overwrite = overwrite)

  # copy file, march
  fs::file_copy(system.file("extdata", "March2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/March2017.CSV.html"), overwrite = overwrite)

  # copy file, april
  fs::file_copy(system.file("extdata", "April2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/April2017.CSV.html"), overwrite = overwrite)

  # copy file, may
  fs::file_copy(system.file("extdata", "May2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/May2017.CSV.html"), overwrite = overwrite)

  # copy file, june
  fs::file_copy(system.file("extdata", "June2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/June2017.CSV.html"), overwrite = overwrite)

  # copy file, july
  fs::file_copy(system.file("extdata", "July2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/July2017.CSV.html"), overwrite = overwrite)

  # copy file, august
  fs::file_copy(system.file("extdata", "August2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/August2017.CSV.html"), overwrite = overwrite)

  # copy file, september
  fs::file_copy(system.file("extdata", "September2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/September2017.CSV.html"), overwrite = overwrite)

  # copy file, october
  fs::file_copy(system.file("extdata", "October2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/October2017.CSV.html"), overwrite = overwrite)

  # copy file, november
  fs::file_copy(system.file("extdata", "November2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/November2017.CSV.html"), overwrite = overwrite)

  # copy file, december
  fs::file_copy(system.file("extdata", "December2017.csv.html", package = "compstatr", mustWork = TRUE),
                paste0(path,"/December2017.CSV.html"), overwrite = overwrite)

}
