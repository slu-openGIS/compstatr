context("test cs_parse_month function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_parse_month( ,CodedMonth,Date,Time), "A existing data frame with data to be separated must be specified for .data"))

test_that("input error triggered - no var", expect_error(cs_parse_month(test_data,,Date,Time), "The column containing the data to be separated must be specified for var"))

test_that("input error triggered - no newMonth", expect_error(cs_parse_month( test_data,CodedMonth,,Year), "The name of the output column containing the year must be specified for newYear"))

test_that("input error triggered - no newYear", expect_error(cs_parse_month(test_data,CodedMonth,Month,),"The name of the output column containing the month must be specified for newMonth" ))

