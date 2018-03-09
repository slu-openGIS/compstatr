# create example data

library(devtools)
library(dplyr)
library(readr)

january2018 <- as_tibble(read_csv("inst/extdata/January2018.csv"))

use_data(january2018, overwrite = TRUE)
