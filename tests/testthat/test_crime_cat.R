context("test cs_crime_cat function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_crime_cat(var = Crime, newVar = crimeCat, output = "string"),
               "An existing data frame with integer crime codes must be specified for .data.")
  expect_error(cs_crime_cat(test_data, newVar = crimeCat, output = "string"),
               "The column containing integer crime codes must be specified for 'var'.")
  expect_error(cs_crime_cat(test_data, var = Crime, output = "string"),
               "The name of the output variable to be created by the function must be specified for 'newVar'.")
  expect_error(cs_crime_cat(test_data, var = Crime, newVar = crimeCat),
               "The type of output must be defined. Options are either 'string', 'factor', or 'numeric'.")
  expect_error(cs_crime_cat(test_data, var = Crime, newVar = crimeCat, output = c("string", "factor")),
               "The output type must be a character scalar. Select one of 'string', 'factor', or 'numeric'.")
  expect_error(cs_crime_cat(test_data, var = Crime, newVar = crimeCat, output = "ham"),
               "The output type must be a character scalar. Select one of 'string', 'factor', or 'numeric'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_crime_cat(test_data, var = Crime, newVar = crimeCat, output = "string"), NA)
})

# test results ------------------------------------------------

results <- cs_crime_cat(test_data, var = Crime, newVar = crimeCat, output = "string")

test_that("correctly specified functions execute without error", {
  expect_equal(class(results$crimeCat), "character")
})

results <- cs_crime_cat(test_data, var = Crime, newVar = crimeCat, output = "factor")

test_that("correctly specified functions execute without error", {
  expect_equal(class(results$crimeCat), "factor")
})

results <- cs_crime_cat(test_data, var = Crime, newVar = crimeCat, output = "numeric")

test_that("correctly specified functions execute without error", {
  expect_equal(class(results$crimeCat), "numeric")
})

