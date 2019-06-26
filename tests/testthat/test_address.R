context("test cs_address function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_address(address = ileads_address, street = ileads_street, newVar = address),
               "An existing data frame with crime data must be specified for '.data'.")
  expect_error(cs_address(test_data, street = ileads_address, newVar = address),
               "The column containing address numbers must be specified for 'address'.")
  expect_error(cs_address(test_data, address = ileads_address, newVar = address),
               "The column containing street names must be specified for 'street'.")
  expect_error(cs_address(test_data, address = ileads_address, street = ileads_street),
               "A new variable name must be specified for 'newVar'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_address(test_data, address = ileads_address, street = ileads_street, newVar = address), NA)
  expect_error(cs_address(test_data, address = "ileads_address", street = "ileads_street", newVar = "address"), NA)
  expect_error(cs_address(test_data, address = cad_address, street = cad_street, newVar = address), NA)
})

# test results ------------------------------------------------

results <- cs_address(test_data, address = ileads_address, street = ileads_street, newVar = address)

test_that("correct output is returned on sample data", {
  expect_equal(results$address[1], "6431 IDAHO AVE")
})
