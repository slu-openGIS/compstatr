## Release summary
`compstatr` was archived from CRAN several weeks ago after ERRORs were found with the new releases of `sf`. Unfortunately, this happened during my transition to work from home and I could not address them in a timely fashion. This version of `compstatr` addresses all prior issues as well as several other bugs.

## Test environments
* local OS X install, R 3.6.1
* Linux xenial distribution (on Travis CI): R-release, R-oldrel, R-devel, and R-3.4.4
* macOS (on Travis CI): R-release, R-oldrel, and R-3.4.4
* windows x64 (on Appveyor): R-release, R-patched, R-oldrel, R-devel, and R-3.4.4
* windows i386 (on Appveyor): R-patched
* winbuilder, R-release, R-oldrel, R-devel

* r-hub not used because it lacks dependencies needed to build `sf` on Debian

## R CMD check results
* There were no ERRORs, WARNINGs, or NOTEs with local or CI checks.

## Reverse dependencies
Not applicable.
