## Release summary
This is an update to the current CRAN version, which adds a number of functions for scraping St. Louis crime data directly from the web. Three new functions (along with associated documentation and unit tests) are introduced. In addition, the 1 NOTE on the CRAN package check results page has been addressed a function from the `tibble` package is used in one of the new functions.

## Test environments
* local OS X install, R 3.6.1
* Linux xenial distribution (on Travis CI): R-release, R-oldrel, R-devel, R-3.4.4, and R-3.3.3
* macOS (on Travis CI): R-release, R-oldrel, R-3.4.4, and R-3.3.3
* windows x64 (on Appveyor): R-release, R-patched, R-oldrel, R-devel, R-3.4.4, and R-3.3.3
* windows i386 (on Appveyor): R-patched
* winbuilder, R-release, R-oldrel, R-devel

* r-hub not used because it lacks dependencies needed to build `sf` on Debian

## R CMD check results
* There were no ERRORs, WARNINGs, or NOTEs with local or CI checks.

## Reverse dependencies
Not applicable.
