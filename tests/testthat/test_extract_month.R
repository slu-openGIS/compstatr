context("test cs_extract_month")

# load data ------------------------------------------------

load(system.file("testdata", "yearList13.rda", package = "compstatr", mustWork = TRUE))

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_extract_month(yearList13, month = 1), NA)
  expect_error(cs_extract_month(yearList13, month = "MARCH"), NA)
  expect_error(cs_extract_month(yearList13, month = "april"), NA)
  expect_error(cs_extract_month(yearList13, month = "May"), NA)
  expect_error(cs_extract_month(yearList13, month = "jun"), NA)
  expect_error(cs_extract_month(yearList13, month = 7), NA)
  expect_error(cs_extract_month(yearList13, month = "8"), NA)
  expect_error(cs_extract_month(yearList13, month = "SEP"), NA)
  expect_error(cs_extract_month(yearList13, month = "Nov"), NA)
  expect_error(cs_extract_month(yearList13, month = "dec"), NA)
  expect_error(cs_extract_month(yearList13, "dec"), NA)
})

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_extract_month(month = "May"),
               "A existing year-list object must be specified for .data.")
  expect_error(cs_extract_month(yearList13),
               "The month to be extracted must be specified.")
  expect_error(cs_extract_month(yearList13, month = "ham"),
               "The given argument for month does not match an acceptible input.")
})

# test results ------------------------------------------------

result1 <- cs_extract_month(yearList13, month = "Feb")

test_that("data is extracted correctly", {
  expect_equal("tbl_df" %in% class(result1), TRUE)
  expect_equal(result1$month_reportedto_mshp[1], "2013-02")
})

result2 <- cs_extract_month(yearList13, month = 10)

test_that("data is extracted correctly", {
  expect_equal("tbl_df" %in% class(result2), TRUE)
  expect_equal(result2$coded_month[1], "2013-10")
})

