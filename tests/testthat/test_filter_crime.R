context("test cs_filter_crime function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_filter_crime(var = count, crime = "homicide"),
               "A existing data frame with data to be subset must be specified for '.data'.")
  expect_error(cs_filter_crime(test_data, crime = "homicide"),
               "The column containing integer crime codes must be specified for 'var'.")
  expect_error(cs_filter_crime(test_data, var = count),
               "The crime to be extracted must be specified with 'crime'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_filter_crime(test_data, var = count, crime = "violent"), NA)
  expect_error(cs_filter_crime(test_data, var = "count", crime = "property"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "part 1"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "homicide"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "rape"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "robbery"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "aggravated assault"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "burglary"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "larceny-theft"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "motor vehicle theft"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "arson"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "part 2"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "other assaults"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "forgery and counterfeiting"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "fraud"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "embezzlement"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "stolen property"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "vandalism"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "weapons"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "prostitution and commercialized vice"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "sex offenses"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "drug abuse violations"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "Gambling"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "offenses against the family and children"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "driving under the influence"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "liquor laws"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "drunkenness"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "disorderly conduct"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "vagrancy"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "all other offenses"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "suspicion"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "curfew and loitering laws-persons under 18"), NA)
  expect_error(cs_filter_crime(test_data, var = count, crime = "runaways-persons under 18"), NA)
})
