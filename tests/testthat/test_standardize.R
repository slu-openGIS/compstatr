context("test cs_standardize function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_standardize(,Feb, 18), "A existing data frame with data to be separated must be specified for .data"))

test_that("input error triggered - no month", expect_error(cs_standardize(test_data,,18), "The column containing the data to be separated must be specified for month"))

test_that("input error triggered - no config", expect_error(cs_standardize(test_data,Feb,), "The non-standard configuration, either 18 or 26 must be specified"))

test_that("input error triggered - not recognized config", expect_error(cs_standardize(test_data,Feb,19), "The given argument for month does not match an acceptible input of 18 or 26."))

test_that("input error triggered - nor recognized month", expect_error(cs_standardize(test_data,brick,18), "The given argument for month does not match an acceptible input."))
