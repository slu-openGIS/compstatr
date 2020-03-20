context("test cs_missingXY function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_projectXY(varX = x_coord, varY = y_coord),
               "An existing data frame with data to be projected must be specified for '.data'.")
  expect_error(cs_projectXY(test_data, varY = y_coord),
               "The column containing the x coordinate must be specified for 'varX'.")
  expect_error(cs_projectXY(test_data, varX = x_coord),
               "The column containing the y coordinate must be specified for 'varY'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_projectXY(test_data, varX = x_coord, varY = y_coord), NA)
  expect_error(cs_projectXY(test_data, varX = "x_coord", varY = "y_coord"), NA)
  expect_error(cs_projectXY(test_data, varX = "x_coord", varY = "y_coord", crs = 4269), NA)
})

# test results ------------------------------------------------

results <- cs_missingXY(test_data, varX = x_coord, varY = y_coord, newVar = missing)
results <- dplyr::filter(results, missing == FALSE)
results <- cs_projectXY(results, varX = x_coord, varY = y_coord, crs = 4269)

test_that("correct output is returned on sample data", {
  expect_equal("sf" %in% class(results), TRUE)
  expect_equal(nrow(results), 3727)
  expect_equal(st_crs(result)$epsg, 4269)
})
