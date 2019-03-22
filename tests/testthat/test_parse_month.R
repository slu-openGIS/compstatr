context("test cs_parse_month function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_parse_month(var = CodedMonth, yearVar = YearCoded, monthVar = MonthCoded),
               "A existing data frame with data to be parsed must be specified for '.data'.")
  expect_error(cs_parse_month(test_data, yearVar = YearCoded, monthVar = MonthCoded),
               "The column containing the data to be parsed must be specified for 'var'.")
  expect_error(cs_parse_month(test_data, var = CodedMonth, monthVar = MonthCoded),
               "The name of the new column containing the year must be specified for 'yearVar'.")
  expect_error(cs_parse_month(test_data, var = CodedMonth, yearVar = YearCoded),
               "The name of the new column containing the month must be specified for 'monthVar'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_parse_month(test_data, var = CodedMonth, yearVar = YearCoded, monthVar = MonthCoded), NA)
  expect_error(cs_parse_month(test_data, var = "CodedMonth", yearVar = "YearCoded", monthVar = "MonthCoded"), NA)
})
