context("test cs_parse_int function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_parse_int( ,ILEADSStreet,Intersection1,Intersection2), "A existing data frame with data to be seperated must be specified for .data"))

test_that("input error triggered - no var", expect_error(cs_parse_int(test_data,ILEADSStreet,Intersection1,Intersection2), "The column containing the data to be separated must be specified for var"))

test_that("input error triggered - no dateVar", expect_error(cs_parse_int( test_data,ILEADSStreet,Intersection1,Intersection2), "The name of the column to be made containing the date information must be specified for dateVar"))

test_that("input error triggered - no timeVar", expect_error(cs_parse_int(test_data ,ILEADSStreet,Intersection1,Intersection2),"The name of the column to be made containing the time information must be specified for timeVar" ))


