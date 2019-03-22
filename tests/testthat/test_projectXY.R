context("test cs_missingXY function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_projectXY(varX = XCoord, varY = YCoord),
               "An existing data frame with data to be projected must be specified for '.data'.")
  expect_error(cs_projectXY(test_data, varY = YCoord),
               "The column containing the x coordinate must be specified for 'varX'.")
  expect_error(cs_projectXY(test_data, varX = XCoord),
               "The column containing the y coordinate must be specified for 'varY'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_projectXY(test_data, varX = XCoord, varY = YCoord), NA)
  expect_error(cs_projectXY(test_data, varX = "XCoord", varY = "YCoord"), NA)
  expect_error(cs_projectXY(test_data, varX = "XCoord", varY = "YCoord", crs = 4269), NA)
})

# test results ------------------------------------------------

results <- cs_missingXY(test_data, varX = XCoord, varY = YCoord, newVar = missing)
results <- dplyr::filter(results, missing == FALSE)
results <- cs_projectXY(results, varX = XCoord, varY = YCoord, crs = 4269)

test_that("correct output is returned on sample data", {
  expect_equal("sf" %in% class(results), TRUE)
  expect_equal(nrow(results), 3727)
  expect_equal(sf::st_crs(results)[[1]], 4269)
})
