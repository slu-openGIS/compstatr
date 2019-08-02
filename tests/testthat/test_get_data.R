context("test cs_get_data function")

# test inputs ------------------------------------------------

i <- "ham"

test_that("misspecified functions return errors", {
  expect_error(cs_get_data(month = "May"),
               "A value for year must be given.")
  expect_error(cs_get_data(year = "ham"),
               "The value for 'year' should be numeric.")
  expect_error(cs_get_data(year = 2007),
               "The earliest year data are available for is 2008.")
})

test_that("misspecified functions return errors", {
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  expect_error(cs_get_data(year = 2008, month = "ham"),
               "The input value for 'month' is not valid.")
  expect_error(cs_get_data(year = 2008, month = "May", index = i),
               "The index object is not properly formatted.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  expect_error(cs_get_data(year = 2008, month = "February"), NA)
  expect_error(cs_get_data(year = 2008, month = "Feb"), NA)
  expect_error(cs_get_data(year = 2008, month = "FEB"), NA)
  expect_error(cs_get_data(year = 2008, month = 2), NA)
  expect_error(cs_get_data(year = 2009), NA)
})
