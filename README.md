
<!-- README.md is generated from README.Rmd. Please edit that file -->
compstatr <img src="man/figures/logo.png" align="right" />
==========================================================

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/compstatr)](https://cran.r-project.org/package=compstatr)

The goal of `compstatr` is to provide a suite of tools for working with crime data made public by the City of St. Louis's [Metropolitan Police Department](http://www.slmpd.org).

Motivation
----------

The St. Louis [Metropolitan Police Department](http://www.slmpd.org) makes it data freely available as a [series of downloadable](http://www.slmpd.org/Crimereports.shtml) `.csv` files. Each file corresponds to a month, beginning with February 2008. Those data are inconsistently organized, with all data before 2013 and some months of 2013 itself having eighteen variables. Beginning during 2013, most months have twenty variables, many of which are named differently from their pre-2014 counterparts. These inconsistencies, and the fact that working with their data requires mangaing over 120 spreadsheets, are the reason for `compstatr`.

Installation
------------

You can install compstatr from Github with:

``` r
# install.packages("devtools")
devtools::install_github("slu-openGIS/compstatr")
```

Opionated Data Preparation
--------------------------

As of March 2018, the City's data is downloaded with an `.html` file extension appended to the filename. The downloaded files look like either `January2008.csv.html` or `January2008.CSV.html`. For our research purposes, we save the raw files by year into a folder called `data/raw/html`. The `html/` folder will therefore have dedicated subfolders like `2008/`, `2009/`, etc.

We then use a bash script called from an `R` notebook to copy all of these to a folder called `data/raw/csv` and then loop through each year's folder, removing the redundent file extension created when `January2008.csv.html` is downloaded. We then use a second loop to change the extension itself to `.csv`.

``` bash
# copy html files new directory for csv
cp -r data/raw/html/* data/raw/csv

# change file extensions
for file in data/raw/csv/2008/*.html
do
  mv "$file" "${file%%.*}.${file##*.}"
done

for file in data/raw/csv/2008/*.html
do
 mv "$file" "${file%.html}.csv"
done
```

The `compstatr` package requires that data are saved in subfolders by year, though the folder hirearchy itself is up to you. It also requires that the files have been renamed from `January2008.csv.html` to `January2008.csv`. If you rename the files futher, the validation scripts will not work. We strongly suggest using a bash script or similar tool to do this in a reproducible, well documented fashion.

Useage
------

### Data Validation

The first step in working with the SLMPD data is to import it. Our approach with `compstatr` is to read into data a year at a time using `cs_load_year()`:

``` r
data2008 <- cs_load_year(here("data", "rawData", "csv", "2008"))
```

The `cs_load_year()` function requires that all 12 months of data are present. It will load data into a single list object containing a tibble for each month. Months are ordered alphabetically, not numerically (see Fixing Validation Errors below).

Once loaded as a list, the `cs_validate_year()` function can be used to determine what issues exist in a given year's data. The validation function will check to make sure that:

1.  `oneMonth` - each object in the list contains one month worth of data,
2.  `valMonth` - each object in the list is appropriately positioned alphabetically,
3.  `varCount` - each object in the list has 20 variables,
4.  `valVars` - if there are 20 variables, that they are named correctly, and
5.  `valClasses` - that each of the 20 variables is the proper class.

The `cs_validate_year()` function returns a tibble that summarizes these results. A `TRUE` value indicates that the given month meets the given condition, and a `FALSE` value means it does not. An `NA` value is reported for `valVars` and `valClasses` if there are not 20 variables in the object.

``` r
cs_validate_year(data2008, year = "2008")
```

### Fixing Valiation Errors - Number of Variables

The simplest validation errors to fix are instances when there are not 20 variables. The `cs_standardize()` function is designed to fix two scenarios - the common instance for pre-2014 data where there are only 18 variables, and a less common instance (only May 2017) where there are 26 variables. This function can be used on an entire list of objects:

``` r
data2008 <- cs_standardize(data2008, config = 18)
```

The `cs_standardize()` function can also be used on a specific month:

``` r
data2017 <- cs_standardize(data2017, month = "May", config = 26)
```

In both cases, it returns a full list of the year's data objects.

### Fixing Validation Errors - Variable Classes

If there are more specific errors, such as mis-formatted columns, a specific month can be extracted using the `cs_extract_month()` function:

Code of Conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
