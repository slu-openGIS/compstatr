context("test cs_crime function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_crime( ,Crime,Crime_True, violent ), "A existing data frame with data to be seperated must be specified for .data"))

test_that("input error triggered - no var", expect_error(cs_crime(test_data,,Crime_True, violent ), "The column containing the data to be separated must be specified for variable"))

test_that("input error triggered - no newVar",expect_error(cs_crime(test_data,Crime,, violent ), "The name of the output variable to be created by the function must be specified for newVar"))

test_that("input error triggered - no crime", expect_error(cs_crime(test_data,Crime,Crime_True,),"A string describing the crime type to be identified must be specified for crime" ))


