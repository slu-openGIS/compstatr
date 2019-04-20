context("test cs_address function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_address(address = ILEADSAddress, street = ILEADSStreet, newVar = address),
               "An existing data frame with crime data must be specified for '.data'.")
  expect_error(cs_address(test_data, street = ILEADSStreet, newVar = address),
               "The column containing address numbers must be specified for 'address'.")
  expect_error(cs_address(test_data, address = ILEADSAddress, newVar = address),
               "The column containing street names must be specified for 'street'.")
  expect_error(cs_address(test_data, address = ILEADSAddress, street = ILEADSStreet),
               "A new variable name must be specified for 'newVar'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_address(test_data, address = ILEADSAddress, street = ILEADSStreet, newVar = address), NA)
  expect_error(cs_address(test_data, address = "ILEADSAddress", street = "ILEADSStreet", newVar = "address"), NA)
  expect_error(cs_address(test_data, address = CADAddress, street = CADStreet, newVar = address), NA)
})

# test results ------------------------------------------------

results <- cs_address(test_data, address = ILEADSAddress, street = ILEADSStreet, newVar = address)

test_that("correct output is returned on sample data", {
  expect_equal(results$address[1], "6431 IDAHO AVE")
})
