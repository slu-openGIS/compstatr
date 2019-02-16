context("test cs_parse_int function")

# load test data

test_data <- january2018

#

test_that("input error triggered - no .data",
          expect_error(cs_parse_int( ,ILEADSStreet,Intersection1,Intersection2),
                       "A existing data frame with data to be separated must be specified for .data"))

test_that("input error triggered - no var",
          expect_error(cs_parse_int(test_data,,Intersection1,Intersection2),
                       "The column containing the data to be separated must be specified for variable"))

test_that("input error triggered - no dateVar",
          expect_error(cs_parse_int( test_data,ILEADSStreet,,Intersection2),
                       "The name of the output column containing a street must be specified for newVar1"))

test_that("input error triggered - no timeVar",
          expect_error(cs_parse_int(test_data ,ILEADSStreet,Intersection1,),
                       "The name of the output column containing a street must be specified for newVar2" ))


