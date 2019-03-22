context("test cs_filter_count function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_filter_count(var = Count),
               "A existing data frame with data to be edited must be specified for '.data'.")
  expect_error(cs_filter_count(test_data),
               "The column containing the data to be edited must be specified for 'var'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_filter_count(test_data, var = Count), NA)
  expect_error(cs_filter_count(test_data, var = "Count"), NA)
})

# test results ------------------------------------------------

results <- cs_filter_count(test_data, var = Count)

test_that("results returned as expected", {
  expect_equal(nrow(results), 3762)
})
