context("test cs_load_year function")

# load data and save items to temp directory ------------------------------------------------
## load data
load(system.file("testdata", "year17.rda", package = "compstatr", mustWork = TRUE))

## create temp directory with data subdir
tmpdir <- tempdir()
fs::dir_create(paste0(tmpdir,"/data2/"))

## write files
readr::write_csv(jan17, path = paste0(tmpdir,"/data2/january2017.csv"))
readr::write_csv(feb17, path = paste0(tmpdir,"/data2/february2017.csv"))
readr::write_csv(mar17, path = paste0(tmpdir,"/data2/march2017.csv"))
readr::write_csv(apr17, path = paste0(tmpdir,"/data2/april2017.csv"))
readr::write_csv(may17, path = paste0(tmpdir,"/data2/may2017.csv"))
readr::write_csv(jun17, path = paste0(tmpdir,"/data2/june2017.csv"))
readr::write_csv(jul17, path = paste0(tmpdir,"/data2/july2017.csv"))
readr::write_csv(aug17, path = paste0(tmpdir,"/data2/august2017.csv"))
readr::write_csv(sep17, path = paste0(tmpdir,"/data2/september2017.csv"))
readr::write_csv(oct17, path = paste0(tmpdir,"/data2/october2017.csv"))
readr::write_csv(nov17, path = paste0(tmpdir,"/data2/november2017.csv"))
readr::write_csv(dec17, path = paste0(tmpdir,"/data2/december2017.csv"))

# test function ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(cs_load_year(path = paste0(tmpdir,"/data2")), NA)
})

# test parameters ------------------------------------------------

## add additional files
readr::write_csv(dec17, path = paste0(tmpdir,"/data2/december2018.csv"))

test_that("too many files trigger error", {
  expect_error(cs_load_year(path = paste0(tmpdir,"/data2")),
               "There are too many files in the specified folder. Load crime files in yearly batches of 12 monthly files.")
})

## delete files
fs::file_delete(paste0(tmpdir,"/data2/december2017.csv"))
fs::file_delete(paste0(tmpdir,"/data2/december2018.csv"))

test_that("too few files triggers warning", {
  expect_warning(cs_load_year(path = paste0(tmpdir,"/data2")),
                 "There are fewer than 12 files in the specified folder. You are only loading a partial year.")
})

# test output ------------------------------------------------

## add additional file back in
readr::write_csv(dec17, path = paste0(tmpdir,"/data2/december2017.csv"))

## create result
result <- cs_load_year(path = paste0(tmpdir,"/data2"))

## create master and result for object names
names_master <- month.name
names_master <- names_master[order(names_master)]

names_result <- names(result)
names_result <- names_result[order(names_result)]

test_that("output is correct", {
  expect_equal(length(result), 12)
  expect_equal("cs_year_list" %in% class(result), TRUE)
  expect_equal("list" %in% class(result), TRUE)
  expect_equal(names_master, names_result)
})
