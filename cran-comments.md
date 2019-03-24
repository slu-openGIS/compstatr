## Release summary
This is the first re-submission of our initial CRAN submission based on feedback from Matthias Sterrer. 

We have made the following changes:

1. add executable examples for `cs_prep_year()`, `cs_load_year()`, and `cs_projectXY`
2. in order to facilitate (1), we have added a `cs_example()` function for creating a sample year worth of `.csv` files
3. clarify `DESCRIPTION` for package based on the feedback we recieved

### Original summary
This package is a compliment to an existing CRAN package, `stlcsb`, which provides access to non-emergency call data. Though this package is focused on St. Louis, it provides access to detailed data about a city with one of the highest violent crime rates in the United States, an issue of interest to researchers both within St. Louis and nationally. It also provides categorization tools that are more broadly applicable to police data from around the United States.

## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on Travis CI), R-release, R-oldrel, R-devel
* macOS (on Travis CI), R-release, R-oldrel
* windows i386 (on Appveyor), R-release, R-oldrel, R-devel
* windows x64 (on Appveyor), R-release, R-oldrel, R-devel
* winbuilder, R-release, R-oldrel, R-devel

## R CMD check results
* There were no ERRORs, WARNINGs, or NOTEs with local checks or on Travis CI/Appveyor.

* winbuilder and `devtools::release()` both note that this is a new submission to CRAN
* winbuilder identified one word that may be misspelled in the `DESCRIPTION` file - it is spelled correctly.

## Reverse dependencies
Not applicable.
