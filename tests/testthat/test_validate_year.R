context("test cs_validate_year function")

# load data ------------------------------------------------

load(system.file("testdata", "yearList13.rda", package = "compstatr", mustWork = TRUE))
load(system.file("testdata", "yearList17.rda", package = "compstatr", mustWork = TRUE))
load(system.file("testdata", "yearList18.rda", package = "compstatr", mustWork = TRUE))

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_equal(cs_validate_year(yearList13, year = 2013), FALSE)
  expect_equal(cs_validate_year(yearList17, year = 2017), FALSE)
  expect_equal(cs_validate_year(yearList18, year = 2018), TRUE)
})

yearList18_prob <- yearList18
yearList18_prob[[3]] <- NULL

test_that("incomplete years (i.e. missing December) are still validated", {
  expect_equal(cs_validate_year(yearList18_prob, year = 2018), TRUE)
})

# test inputs ------------------------------------------------

test_that("incorrectly specified return FALSE when year is mismatched", {
  expect_equal(cs_validate_year(yearList13, year = 2008), FALSE)
})

# test problems with year-list object ------------------------------------------------

## January is missing
yearList18_prob <- yearList18
yearList18_prob[[5]] <- NULL

test_that("missing January is detected", {
  expect_warning(cs_validate_year(yearList18_prob, year = 2018),
                 "The given year list object does not include January.")
})

## April is missing
yearList18_prob <- yearList18
yearList18_prob[[1]] <- NULL

test_that("missing January is detected", {
  expect_warning(cs_validate_year(yearList18_prob, year = 2018),
                 "The given year list object does not contain all consecutive months between January and the last given month.")
})

## Duplicate month
## this is not correctly identifying the problem
yearList18_prob <- yearList18
yearList18_prob[[3]] <- NULL
yearList18_prob[[12]] <- yearList18_prob[[11]]

test_that("duplicate is detected", {
  expect_warning(cs_validate_year(yearList18_prob, year = 2018),
                 "The given year list object does not contain all consecutive months between January and the last given month.")
})

# test result ------------------------------------------------

result13 <- cs_validate_year(yearList13, year = 2013, verbose = TRUE)
result17 <- cs_validate_year(yearList17, year = 2013, verbose = TRUE)

test_that("problems with variables are identified", {
  expect_equal(all(result13$varCount), FALSE)
  expect_equal(all(result17$varCount), FALSE)
})
