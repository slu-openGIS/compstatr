context("test cs_filter_crime function")

# load data ------------------------------------------------

## load january 2018 data
test_data <- january2018

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_filter_crime(var = Count, crime = "homicide"),
               "A existing data frame with data to be subset must be specified for '.data'.")
  expect_error(cs_filter_crime(test_data, crime = "homicide"),
               "The column containing integer crime codes must be specified for 'var'.")
  expect_error(cs_filter_crime(test_data, var = Count),
               "The crime to be extracted must be specified with 'crime'.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_filter_crime(test_data, var = Count, crime = "violent"), NA)
  expect_error(cs_filter_crime(test_data, var = "Count", crime = "property"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "part 1"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "homicide"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "rape"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "robbery"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "aggravated assault"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "burglary"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "larceny-theft"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "motor vehicle theft"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "arson"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "part 2"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "other assaults"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "forgery and counterfeiting"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "fraud"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "embezzlement"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "stolen property"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "vandalism"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "weapons"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "prostitution and commercialized vice"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "sex offenses"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "drug abuse violations"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "Gambling"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "offenses against the family and children"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "driving under the influence"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "liquor laws"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "drunkenness"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "disorderly conduct"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "vagrancy"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "all other offenses"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "suspicion"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "curfew and loitering laws-persons under 18"), NA)
  expect_error(cs_filter_crime(test_data, var = Count, crime = "runaways-persons under 18"), NA)
})
