context("test cs_is_int function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_is_int( ,ILEADSStreet,new_column), "A existing data frame with data to be seperated must be specified for .data"))
test_that("input error triggered - no variable specified", expect_error(cs_is_int(test_data,,intersection), "The column containing the data to be separated must be specified for variable"))
