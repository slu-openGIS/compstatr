context("test cs_parse_date function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_parse_date(var = DateOccur, dateVar = date, timeVar = time),
               "A existing data frame with data to be parsed must be specified for '.data'.")
  expect_error(cs_parse_date(test_data, dateVar = date, timeVar = time),
               "The column containing the data to be parsed must be specified for 'var'.")
  expect_error(cs_parse_date(test_data, var = DateOccur, timeVar = time),
               "The name of the new column to be made containing the date must be specified for 'dateVar'.")
  expect_error(cs_parse_date(test_data, var = DateOccur, dateVar = date),
               "The name of the new column to be made containing the time must be specified for 'timeVar'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_parse_date(test_data, var = DateOccur, dateVar = date, timeVar = time), NA)
  expect_error(cs_parse_date(test_data, var = "DateOccur", dateVar = "date", timeVar = "time"), NA)
  expect_error(cs_parse_date(test_data, var = DateOccur, dateVar = date, timeVar = time, keepDateTime = TRUE), NA)
})
