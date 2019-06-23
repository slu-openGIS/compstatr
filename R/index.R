#' Create Index of Available Months
#'
#' @description Constructs a table for finding a given table of crime data or a set of
#'     tables (such as year to date or full year). This is largely needed for internal
#'     use when downloading tables, but is exported for reference and troubleshooting.
#'
#' @usage cs_create_index()
#'
#' @return A tibble with all available monthly crime tables, the \code{iframe} page
#'     they appear on, and their row number.
#'
#' @importFrom dplyr mutate select
#' @importFrom httr POST warn_for_status
#' @importFrom purrr map_df
#' @importFrom rvest html_form html_nodes html_session html_table
#' @importFrom tibble tibble
#' @importFrom utils head
#'
#' @export
cs_create_index <- function(){

  # global bindings
  filename = month = value = NULL

  # url
  url <- "http://www.slmpd.org/CrimeReport.aspx"

  # create session
  page <- rvest::html_session(url)

  # construct form
  form <- rvest::html_form(page)[[1]]

  # create vector of page numbers
  pages <- c(1:cs_count_pages())

  # iterate over iframe pages
  pages %>%
    purrr::map_df(~cs_get_table(url = url, session = page, form = form, page = .x)) -> out

  # construct output
  out <- dplyr::mutate(out, date = stringr::str_sub(filename, 1, stringr::str_length(filename)-4))
  out <- dplyr::mutate(out, month = stringr::str_sub(date, 1, stringr::str_length(date)-4))
  out <- dplyr::mutate(out, year = as.numeric(stringr::str_sub(date, -4)))
  out <- dplyr::mutate(out, date = paste(month, year))
  out <- dplyr::mutate(out, month = match(month, month.name))
  out <- dplyr::mutate(out, value = paste(page, row))
  out <- dplyr::select(out, page, row, value, year, month, date)

  # return output
  return(out)

}


cs_get_table <- function(url, session, form, page){

  # global bindings
  filename = NULL

  # update session to correct page
  if (page > 1){
    session <- cs_switch_page(url = url, session = session, form = form, page = page)
  }

  # scrape table
  tbl <- rvest::html_nodes(session, "table")[[1]]
  tbl <- rvest::html_table(tbl, fill = TRUE)
  tbl <- as.data.frame(tbl)

  # create vector of file names
  vctr <- tbl$`Crime Detail`

  # remove last two entries from vector
  vctr <- utils::head(vctr, -2)

  # calculate law row position
  last <- length(vctr)+1

  # construct output
  out <- tibble::tibble(
    filename = vctr,
    row = c(2:last)
  )

  out <- dplyr::mutate(out, row = formatC(row, width = 2, format = "d", flag = "0"))
  out <- dplyr::mutate(out, page = page)
  out <- dplyr::select(out, page, row, filename)

  # return output
  return(out)

}

cs_switch_page <- function(url, session, form, page){

  # construct event argument value
  arg <- paste0("Page$",page)

  # update session
  session <- cs_request_POST(session, url, body = list(
    `__EVENTTARGET`="GridView1",
    `__EVENTARGUMENT`=arg,
    `__VIEWSTATE`=form$fields$`__VIEWSTATE`$value,
    `__VIEWSTATEGENERATOR`=form$fields$`__VIEWSTATEGENERATOR`$value,
    `__EVENTVALIDATION`=form$fields$`__EVENTVALIDATION`$value
  ))

  # return output
  return(session)

}

# included since it is not exported from rvest
cs_request_POST <- function(x, url, ...) {
  x$response <- httr::POST(url, x$config, ..., handle = x$handle)
  x$html <- new.env(parent = emptyenv(), hash = FALSE)
  x$url <- x$response$url
  x$back <- character() # can't go back after a post

  httr::warn_for_status(x$response)
  x
}
