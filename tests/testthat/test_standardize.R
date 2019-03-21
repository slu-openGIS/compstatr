context("test cs_standardize function")

# load data ------------------------------------------------

load(system.file("testdata", "yearList13.rda", package = "compstatr", mustWork = TRUE))
load(system.file("testdata", "yearList17.rda", package = "compstatr", mustWork = TRUE))

# extract data ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_equal(ncol(cs_extract_month(yearList13, month = "May")), 18)
  expect_equal(ncol(cs_extract_month(yearList17, month = "May")), 26)
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_standardize(yearList13, month = 1, config = 18), NA)
  expect_error(cs_standardize(yearList13, month = "FEB", config = 18), NA)
  expect_error(cs_standardize(yearList13, month = "March", config = 18), NA)
  expect_error(cs_standardize(yearList13, month = "apr", config = 18), NA)
  expect_error(cs_standardize(yearList13, month = 8, config = 18), NA)
  expect_error(cs_standardize(yearList17, month = "May", config = 26), NA)
})

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_standardize(month = "May", config = 18),
               "A existing year-list object must be specified for .data.")
  expect_error(cs_standardize(yearList13, config = 18),
               "The month to be standardized must be specified.")
  expect_error(cs_standardize(yearList13, month = "ham", config = 18),
               "The given argument for month does not match an acceptible input.")
  expect_error(cs_standardize(yearList13, month = 5),
               "The non-standard configuration, either 18 or 26 must be specified.")
  expect_error(cs_standardize(yearList13, month = 5, config = 85),
               "The given argument for 'config' does not match an acceptible input of '18' or '26'.")
})

# test results ------------------------------------------------

yearList13 <- cs_standardize(yearList13, month = "May", config = 18)
yearList17 <- cs_standardize(yearList17, month = "May", config = 26)

test_that("column numbers are corrected", {
  expect_equal(ncol(cs_extract_month(yearList13, month = "May")), 20)
  expect_equal(ncol(cs_extract_month(yearList17, month = "May")), 20)
})
