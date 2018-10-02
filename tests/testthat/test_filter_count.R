context("test cs_filter_count function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_filter_count( ,Count), "A existing data frame with data to be seperated must be specified for .data"))

test_that("input error triggered - no column specified", expect_error(cs_filter_count(test_data,), "The column containing the data to be separated must be specified for variable"))
