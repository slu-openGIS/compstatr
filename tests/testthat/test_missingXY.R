context("test cs_missingXY function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_missingXY( ,XCoord,YCoord,missingXY), "A existing data frame with data to be separated must be specified for .data"))

test_that("input error triggered - no varx", expect_error(cs_missingXY(test_data,,YCoord,missingXY), "The column containing the data to be separated must be specified for varx"))

test_that("input error triggered - no varx", expect_error(cs_missingXY( test_data,XCoord,,missingXY), "The column containing the data to be separated must be specified for vary"))

test_that("input error triggered - no newVar", expect_error(cs_missingXY(test_data ,XCoord,YCoord,),"The output column name must be specified for newVar" ))


