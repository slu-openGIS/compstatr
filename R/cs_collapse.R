#'
cs_collapse <- function(.data){
  
  jan <- cs_extract_month(.data, month = 1)
  feb <- cs_extract_month(.data, month = 2)
  mar <- cs_extract_month(.data, month = 3)
  apr <- cs_extract_month(.data, month = 4)
  may <- cs_extract_month(.data, month = 5)
  june <- cs_extract_month(.data, month = 6)
  july <- cs_extract_month(.data, month = 7)
  aug <- cs_extract_month(.data, month = 8)
  sept <- cs_extract_month(.data, month = 9)
  oct <- cs_extract_month(.data, month = 10)
  nov <- cs_extract_month(.data, month = 11)
  dec <- cs_extract_month(.data, month = 12)
  
  year <- bind_rows(jan, feb, mar, apr, may, june, july, aug, sept, oct, nov, dec)
  
  return(year)
  
}
