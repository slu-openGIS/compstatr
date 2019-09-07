# compstatr 0.2.1.9000

* address bug where categorize and filter functions failed when the `var` argument was not an integer variable

# compstatr 0.2.0

* add functionality for directly scraping crime data into `R`:
    * add `cs_last_update` - checks date of last data release by SLMPD
    * add `cs_create_index` - creates a full tibble of available month tables
    * add `cs_get_data` - downloads a month or year worth of crime tables
    * There is one breaking change for users - to enable these functions, all variable names are standardized into `snake_case` at import. This will impact code that has been written using a prior version of `compstatr`.
* update `README` and `index.Rmd` with "What's New?" sections

# compstatr 0.1.1

* **initial CRAN submission**
* add Zenodo release badge to `README`
* update installation instructions on `README` and on the `pkgdown` site
* add `cran-comments.md`
* in response to initial feedback from Matthias Sterrer (CRAN):
    * add `cs_example()` function for creating a sample year worth of `.csv` files 
    * add executable examples for `cs_prep_year()`, `cs_load_year()`, and `cs_projectXY`
    * clarify `DESCRIPTION` for package
* in response to additional feedback from Swetlana Herbrandt (CRAN):
    * added a link to the `DESCRIPTION` file to the SLMPD data source
    * ensured that the examples in `example.R` and `create.R` read from and write to a temporary directory
* add a function `cs_address()` to facilitate concatenation of street addresses prior to geocoding

# compstatr 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Initial package development including package infrastructure, functions, and sample data
