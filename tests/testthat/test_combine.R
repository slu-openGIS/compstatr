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

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_combine(type = "year", date = 2017, yearList17, yearList18), NA)
})
