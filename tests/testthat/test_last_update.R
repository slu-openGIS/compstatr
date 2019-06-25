context("test cs_last_update function")

# test inputs ------------------------------------------------

test_that("misspecified functions return errors", {
  expect_error(cs_last_update(output = "ham"),
               "The only accepted arguments for 'cs_last_update' are 'string' and 'date'.")
})

# test results ------------------------------------------------

result1 <- cs_last_update(output = "string")
result2 <- cs_last_update(output = "date")

test_that("correctly specified functions execute without error", {
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  expect_equal(class(result1), "character")
  expect_equal(class(result2), "Date")
})
