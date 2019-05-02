context("test cs_prep_year function")

## clean-up temp directories
tmpdir <- tempdir()
unlink(tmpdir, recursive = TRUE)

# load data and save items to temp directory ------------------------------------------------
## load data
load(system.file("testdata", "year17.rda", package = "compstatr", mustWork = TRUE))

## create temp directory with data subdir
tmpdir <- tempdir()
fs::dir_create(paste0(tmpdir,"/data/"))

## write files
cs_example(path = paste0(tmpdir, "/data/"))

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_prep_year(path = paste0(tmpdir,"/data/")), NA)
})

## remove tempdir
unlink(tmpdir, recursive = TRUE)

## create temp directory with data subdir
tmpdir <- tempdir()
fs::dir_create(paste0(tmpdir,"/data/"))

## write files
cs_example(path = paste0(tmpdir, "/data/"))

test_that("correctly specified functions execute without error", {
  expect_error(cs_prep_year(path = paste0(tmpdir,"/data/")), NA)
})

# test output ------------------------------------------------

## load data
load(system.file("testdata", "prepResults.rda", package = "compstatr", mustWork = TRUE))

## remove tempdir
unlink(tmpdir, recursive = TRUE)

## create temp directory with data subdir
tmpdir <- tempdir()
fs::dir_create(paste0(tmpdir,"/data/"))

## write files
cs_example(path = paste0(tmpdir, "/data/"))

## create results
results <- cs_prep_year(path = paste0(tmpdir,"/data/"), verbose = TRUE)

## compare
test_that("results created correctly", {
  expect_equal(prepResults$new, results$new)
})

# test parameters ------------------------------------------------

test_that("incorrect parameters trigger error", {
  expect_error(cs_prep_year(path = paste0(tmpdir,"/data/"), verbose = "ham"),
               "The 'verbose' parameter only accepts 'TRUE' or 'FALSE' as valid arguments.")
})

## remove tempdir
unlink(tmpdir, recursive = TRUE)

## create temp directory with data subdir
tmpdir <- tempdir()
fs::dir_create(paste0(tmpdir,"/data/"))

## write files
cs_example(path = paste0(tmpdir, "/data/"))

## add additional files
readr::write_csv(dec17, path = paste0(tmpdir,"/data/december2018.CSV.html"))

test_that("too many files trigger error", {
  expect_error(cs_prep_year(path = paste0(tmpdir,"/data/")),
               "There are too many files in the specified folder. Edit crime files in yearly batches of 12 monthly files.")
})

## delete files
fs::file_delete(paste0(tmpdir,"/data/December2017.CSV.html"))
fs::file_delete(paste0(tmpdir,"/data/december2018.CSV.html"))

test_that("too few files triggers warning", {
  expect_warning(cs_prep_year(path = paste0(tmpdir,"/data/")),
                 "There are fewer than 12 files in the specified folder. You are only editing a partial year.")
})

# final options ------------------------------------------------

unlink(tmpdir, recursive = TRUE)
