
# compstatr <img src="man/figures/logo.png" align="right" />
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis-CI Build Status](https://travis-ci.org/slu-openGIS/compstatr.svg?branch=master)](https://travis-ci.org/slu-openGIS/compstatr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/slu-openGIS/compstatr?branch=master&svg=true)](https://ci.appveyor.com/project/chris-prener/compstatr)
[![Coverage status](https://codecov.io/gh/slu-openGIS/compstatr/branch/master/graph/badge.svg)](https://codecov.io/github/slu-openGIS/compstatr?branch=master)
[![CRAN_status_badge](http://www.r-pkg.org/badges/version/compstatr)](https://cran.r-project.org/package=compstatr)
[![cran checks](https://cranchecks.info/badges/worst/compstatr)](https://cran.r-project.org/web/checks/check_results_compstatr.html)
[![DOI](https://zenodo.org/badge/105331568.svg)](https://zenodo.org/badge/latestdoi/105331568)

The goal of `compstatr` is to provide a suite of tools for working with crime data made public by the City of St. Louis' [Metropolitan Police Department](http://www.slmpd.org).

## What's New in v0.2.1?
Version v0.2.1 addresses a number of bugs, and returns `compstatr` to CRAN.

## Quick Start
If the `sf` package is already installed, the easiest way to get `compstatr` is to install it from CRAN:

``` r
install.packages("compstatr")
```

The development version of `compstatr` can be accessed from GitHub with `remotes`:

```r
# install.packages("remotes")
remotes::install_github("slu-openGIS/compstatr")
```

Additional details, including some tips for installing `sf`, can be found in the [Get started article](articles/compstatr.html#installation).

## Resources
In addition to instructions for installation, the main [Get started](articles/compstatr.html) article has:

  * some tips on installing `compstatr` and its dependencies,
  * a quick overview of data acquisition,
  * an overview of the functions available in `compstatr`,
  * and a full example of the `compstatr` workflow along with some basic mapping of these data.

## Acknowledgements
We wish to thank Taylor Braswell for his significant efforts compiling Stata code early in this project. Taylor's code was used as a reference when developing this package, and many of the functions reflect issues that he worked to identify.

## Contributor Code of Conduct
Please note that this project is released with a [Contributor Code of Conduct](https://slu-opengis.github.io/compstatr/CODE_OF_CONDUCT.html). By participating in this project you agree to abide by its terms.
