# create example data

library(devtools)
library(readr)

january2018 <- read_csv("inst/extdata/January2018.csv")

use_data(january2018)
