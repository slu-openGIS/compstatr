#' Download Crime Data from SLMPD
#'
#' @description Downloads crime data from the SLMPD website.
#'
#' @usage cs_get_data(year, month)
#'
#' @param year A year value in the style \code{YYYY}
#' @param month Optional; a month number
#' @param index Optional; an index object created with \code{\link{cs_create_index}}.
#'     Building the index prior to downloading data, especially if you are downloading
#'     multiple years worth of data, will result in dramatically faster execution
#'     times for this function.
#'
#' @return A year-list object ready for validation.
#'
#' @importFrom dplyr %>% as_tibble filter mutate mutate_all
#' @importFrom httr content
#' @importFrom purrr map
#' @importFrom rvest html_form html_session
#' @importFrom stringr word
#' @importFrom utils read.csv
#'
#' @export
cs_get_data <- function(year, month, index){

  # optionally build index
  if (missing(index)){
    index <- cs_create_index()
  }

  # rename year and month
  x <- year

  # subset index
  if (missing(month)){
    index <- dplyr::filter(index, year == x)
  } else {
    y <- month
    index <- dplyr::filter(index, year == x & month == y)
  }

  # url
  url <- "http://www.slmpd.org/CrimeReport.aspx"

  # create session
  page <- rvest::html_session(url)

  # construct form
  form <- rvest::html_form(page)[[1]]

  # download
  if (missing(month) == TRUE){

    # store value
    value <- index$value

    # iterate
    value %>%
      unlist(value) %>%
      purrr::map(~cs_download(value = .x, url = url, session = page, form = form)) -> out

    # create list of months associated with year list object items
    out %>%
      purrr::map(cs_identifyMonth) -> nameList

    # convert list of months to vector
    nameVector <- unlist(nameList, recursive = TRUE, use.names = TRUE)

    # apply vector to data
    names(out) <- nameVector

  } else if (missing(month) == FALSE){

    # store value
    value <- index$value[[1]]

    # pull table
    out <- cs_download(value = value, url = url, session = page, form = form)

  }

  # return output
  return(out)

}

cs_download <- function(value, url, session, form){

  # parse value
  page <- stringr::word(value, 1)
  row <- stringr::word(value, 2)

  # update session to correct page
  if (page > 1){
    session <- cs_switch_page(url = url, session = session, form = form, page = page)
    form <- rvest::html_form(session)[[1]]
  }

  # construct target
  target <- paste0("GridView1$ctl", row, "$lnkdownloadD")

  # generate response
  response <- cs_request_POST(session, url, body = list(
    `__EVENTTARGET`=target,
    `__EVENTARGUMENT`="",
    `__VIEWSTATE`=form$fields$`__VIEWSTATE`$value,
    `__VIEWSTATEGENERATOR`=form$fields$`__VIEWSTATEGENERATOR`$value,
    `__EVENTVALIDATION`=form$fields$`__EVENTVALIDATION`$value
  ))

  # generate output
  text <- response$response
  out <- utils::read.csv(textConnection(
    suppressMessages(httr::content(text, as = "text", encoding = "ISO-8859-1"))),
    stringsAsFactors = FALSE, na.strings=c(""," "))
  out <- dplyr::as_tibble(out)

  # convert all columns to character (to match on disk workflow)
  out <- dplyr::mutate_all(out, as.character)

  # clean-up variable names
  out <- janitor::clean_names(out)

  # search for i_complaint and correct
  if ("i_complaint" %in% names(out) == TRUE){
    out <- dplyr::rename(out, complaint = i_complaint)
  }

  # return output
  return(out)

}
