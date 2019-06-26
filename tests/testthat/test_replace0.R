context("test cs_replace0 function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_replace0(var = x_coord),
               "An existing data frame with data to be edited must be specified for '.data'.")
  expect_error(cs_replace0(test_data),
               "The column containing coordinate data must be specified for 'var'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_replace0(test_data, var = x_coord), NA)
  expect_error(cs_replace0(test_data, var = "y_coord"), NA)
})

# test results ------------------------------------------------

results <- cs_replace0(test_data, var = x_coord)
results <- cs_replace0(results, var = y_coord)

test_that("correct output is returned on sample data", {
  expect_equal(sum(is.na(results$x_coord)), 98)
  expect_equal(sum(is.na(results$y_coord)), 98)
})
