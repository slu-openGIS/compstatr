context("test cs_extract_month function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_extract_month( ,jan), "A existing data frame with data to be seperated must be specified for .data"))

test_that("input error triggered - no month", expect_error(cs_extract_month(test_data,), "The month to be extracted must be specified."))

test_that("input error triggered - not recognized month",expect_error(cs_extract_month(test_data,brick), "The given argument for month does not match an acceptible input."))



