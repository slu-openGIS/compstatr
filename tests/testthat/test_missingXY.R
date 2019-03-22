context("test cs_missingXY function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_missingXY(varX = XCoord, varY = YCoord, newVar = missing),
               "An existing data frame with data to be analyzed must be specified for '.data'.")
  expect_error(cs_missingXY(test_data, varY = YCoord, newVar = missing),
               "The column containing the x coordinate must be specified for 'varX'.")
  expect_error(cs_missingXY(test_data, varX = XCoord, newVar = missing),
               "The column containing the y coordinate must be specified for 'varY'.")
  expect_error(cs_missingXY(test_data, varX = XCoord, varY = YCoord),
               "The output column name must be specified for 'newVar'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_missingXY(test_data, varX = XCoord, varY = YCoord, newVar = missing), NA)
  expect_error(cs_missingXY(test_data, varX = "XCoord", varY = "YCoord", newVar = "missing"), NA)
})

# test results ------------------------------------------------

results <- cs_missingXY(test_data, varX = XCoord, varY = YCoord, newVar = missing)
freq <- unname(table(results$missing))

test_that("correct output is returned on sample data", {
  expect_equal(freq[1], 3727) # test number of valid coordinate pairs
  expect_equal(freq[2], 98) # test number of missing coordinate pairs
})
