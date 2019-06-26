context("test cs_crime function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_crime(var = crime, newVar = violent, crime = "violent"),
               "An existing data frame with integer crime codes must be specified for .data.")
  expect_error(cs_crime(test_data, newVar = violent, crime = violent),
               "The column containing integer crime codes must be specified for 'var'.")
  expect_error(cs_crime(test_data, var = crime, crime = violent),
               "The name of the output variable to be created by the function must be specified for 'newVar'.")
  expect_error(cs_crime(test_data, var = crime, newVar = violent),
               "A string describing the crime type to be identified must be specified for 'crime'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "violent"), NA)
  expect_error(cs_crime(test_data, var = "crime", newVar = "test", crime = "property"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "part 1"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "homicide"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "rape"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "robbery"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "aggravated assault"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "burglary"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "larceny-theft"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "motor vehicle theft"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "arson"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "part 2"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "other assaults"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "forgery and counterfeiting"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "fraud"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "embezzlement"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "stolen property"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "vandalism"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "weapons"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "prostitution and commercialized vice"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "sex offenses"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "drug abuse violations"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "Gambling"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "offenses against the family and children"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "driving under the influence"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "liquor laws"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "drunkenness"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "disorderly conduct"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "vagrancy"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "all other offenses"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "suspicion"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "curfew and loitering laws-persons under 18"), NA)
  expect_error(cs_crime(test_data, var = crime, newVar = test, crime = "runaways-persons under 18"), NA)
})

# test results ------------------------------------------------

results <- cs_crime(test_data, var = crime, newVar = violent, crime = "violent")

test_that("correctly specified functions execute without error", {
  expect_equal(class(results$violent), "logical")
})
