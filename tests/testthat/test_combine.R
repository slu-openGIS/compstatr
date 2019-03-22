context("test cs_combine")

# load data ------------------------------------------------

## load and standardize 2017 data
load(system.file("testdata", "yearList17.rda", package = "compstatr", mustWork = TRUE))
yearList17 <- cs_standardize(yearList17, month = "May", config = 26)
yearList17 <- cs_collapse(yearList17)

## load 2018 data
load(system.file("testdata", "yearList18.rda", package = "compstatr", mustWork = TRUE))
yearList18 <- cs_collapse(yearList18)

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_combine(type = "ham", date = 2017, yearList17, yearList18),
               "The output type must be a character scalar. Select one of 'year' or 'ytd'.")
  expect_error(cs_combine(type = c("year", "ytd"), date = 2017, yearList17, yearList18),
               "The output type must be a character scalar. Select one of 'year' or 'ytd'.")
  expect_error(cs_combine(date = 2017, yearList17, yearList18),
               "A timeframe must be given for 'type'. At this time, only 'year' is a valid argument.")
  expect_error(cs_combine(type = "year", yearList17, yearList18),
               "An integer year must be given for 'date'.")
  expect_error(cs_combine(yearList17, yearList18),
               "A timeframe must be given for 'type'. At this time, only 'year' is a valid argument.")
  expect_error(cs_combine(yearList17),
               "A timeframe must be given for 'type'. At this time, only 'year' is a valid argument.")
})

test_that("ytd returns error", {
  expect_error(cs_combine(type = "ytd", date = 2017, yearList17, yearList18),
               "This functionality is still in progress.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_combine(type = "year", date = 2017, yearList17, yearList18), NA)
  expect_error(cs_combine("year", 2017, yearList17, yearList18), NA)
})
