context("test cs_parse_date function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_parse_date( ,dateOccur,Date,Time), "A existing data frame with data to be separated must be specified for .data"))

test_that("input error triggered - no var", expect_error(cs_parse_date(test_data,,Date,Time), "The column containing the data to be separated must be specified for var"))

test_that("input error triggered - no dateVar", expect_error(cs_parse_date( test_data,dateOccur,,Time), "The name of the column to be made containing the date information must be specified for dateVar"))

test_that("input error triggered - no timeVar", expect_error(cs_parse_date(test_data,dateOccur,Date,),"The name of the column to be made containing the time information must be specified for timeVar" ))


