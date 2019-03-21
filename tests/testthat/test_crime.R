context("test cs_crime function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_crime(var = Crime, newVar = violent, crime = "violent"),
               "A existing data frame with data to be seperated must be specified for .data.")
  expect_error(cs_crime(test_data, newVar = violent, crime = violent),
               "The column containing the data to be separated must be specified for 'var'.")
  expect_error(cs_crime(test_data, var = Crime, crime = violent),
               "The name of the output variable to be created by the function must be specified for 'newVar'.")
  expect_error(cs_crime(test_data, var = Crime, newVar = violent),
               "A string describing the crime type to be identified must be specified for 'crime'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_crime(test_data, var = Crime, newVar = violent, crime = "violent"), NA)
  expect_error(cs_crime(test_data, var = Crime, newVar = property, crime = "property"), NA)
  expect_error(cs_crime(test_data, var = Crime, newVar = p1, crime = "part 1"), NA)
  expect_error(cs_crime(test_data, var = Crime, newVar = homicide, crime = "homicide"), NA)
  expect_error(cs_crime(test_data, var = Crime, newVar = rape, crime = "rape"), NA)
  expect_error(cs_crime(test_data, var = Crime, newVar = robbery, crime = "robbery"), NA)
})

# test results ------------------------------------------------

results <- cs_crime(test_data, var = Crime, newVar = violent, crime = "violent")

test_that("correctly specified functions execute without error", {
  expect_equal(class(results$violent), "logical")
})
