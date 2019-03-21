context("test cs_replace0 function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_replace0(,XCoord), "A existing data frame with data to be separated must be specified for .data."))

test_that("input error triggered - no var", expect_error(cs_replace0(test_data,), "The column containing coordinate data must be specified."))
