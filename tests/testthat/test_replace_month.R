context("test cs_replace_month")

# load data ------------------------------------------------

## load 2013 data
load(system.file("testdata", "yearList13.rda", package = "compstatr", mustWork = TRUE))

## create month for testing
month <- cs_extract_month(yearList13, month = 1)

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_replace_month(month = "May", monthData = month),
               "A existing year-list object must be specified for .data.")
  expect_error(cs_replace_month(yearList13, monthData = month),
               "The month to be replaced must be specified.")
  expect_error(cs_replace_month(yearList13, month = "ham", monthData = month),
               "The given argument for month does not match an acceptible input.")
  expect_error(cs_replace_month(yearList13, month = "May"),
               "The month object to be replaced must be specified.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_replace_month(yearList13, month = 1, monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "MARCH", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "april", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "May", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "jun", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = 7, monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "8", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "SEP", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "October", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "Nov", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, month = "dec", monthData = month), NA)
  expect_error(cs_replace_month(yearList13, "dec", month), NA)
})
