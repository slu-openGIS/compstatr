context("test cs_crime_cat function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_crime_cat( ,Crime,Crime_True,"string"), "A existing data frame with data to be seperated must be specified for .data"))

test_that("input error triggered - no var", expect_error(cs_crime_cat(test_data,,Crime_True,"string"), "The column containing the data to be separated must be specified for variable"))

test_that("input error triggered - no newVar",expect_error(cs_crime_cat(test_data,Crime,,"string"), "The name of the output variable to be created by the function must be specified for newVar"))

test_that("input error triggered - no output specified", expect_error(cs_crime_cat(test_data,Crime,Crime_True,),"The type of output must be defined - either string, factor, or numeric" ))


