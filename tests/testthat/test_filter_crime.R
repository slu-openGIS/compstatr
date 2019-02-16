context("test cs_filter_crime function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data", expect_error(cs_filter_crime( ,Crime,violent), "A existing data frame with data to be seperated must be specified for .data"))

test_that("input error triggered - no crime", expect_error(cs_filter_crime(test_data,,violent), "The column containing the crime data for var must be specified"))

test_that("input error triggered - no crime", expect_error(cs_filter_crime(test_data,Crime,), "The crime to be extracted must be specified."))

test_that("input error triggered - not a recognized crime",expect_error(cs_filter_crime(test_data,Crime,brick), "The given argument for crime does not match an acceptible input."))

