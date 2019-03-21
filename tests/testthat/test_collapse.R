context("test cs_collapse")

# load data ------------------------------------------------

## load and standardize 2017 data
load(system.file("testdata", "yearList17.rda", package = "compstatr", mustWork = TRUE))
yearList17 <- cs_standardize(yearList17, month = "May", config = 26)

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_replace_month(),
               "A existing year-list object must be specified for .data.")
})

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_collapse(yearList17), NA)
})
