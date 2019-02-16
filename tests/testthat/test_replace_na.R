context("test cs_replace_na function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_replace_na(,XCoord), "A existing data frame with data to be separated must be specified for .data"))

test_that("input error triggered - no var", expect_error(cs_replace_na(test_data,), "The column containing the data to be separated must be specified for variable"))
