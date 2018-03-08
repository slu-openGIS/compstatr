library(dplyr)
library(tidyr)
library(rlang)

x <- read.csv("C:/Users/cree1/Downloads/August2017.CSV")


# save parameters to list
paramList <- as.list(match.call())

#quote input variables
varN <- rlang::quo_name(rlang::enquo(Crime))
if (!is.character(paramList$Crime)) {
  var <- rlang::enquo(Crime)
} else if (is.character(paramList$Crime)) {
  var <- rlang::quo(!! rlang::sym(Crime))
}

y <- x %>%
  mutate(tester = case_when(Crime >= 10000 && Crime < 20000 ~ "Criminal Homicide",
                            Crime >= 20000 && Crime < 30000 ~ "Forcible Rape",
                            Crime >= 30000 && Crime < 40000 ~ "Robbery",
                            Crime >= 40000 && Crime < 50000 ~ "Aggravated Assault",
                            Crime >= 50000 && Crime < 60000 ~ "Burgalry",
                            Crime >= 60000 && Crime < 70000 ~ "Larceny-theft",
                            Crime >= 70000 && Crime < 80000 ~ "Motor Vehicle Theft",
                            Crime >= 80000 && Crime < 90000 ~ "Arson",
                            Crime >= 90000 && Crime < 100000 ~ "Other Assaults",
                            Crime >= 100000 && Crime < 110000 ~ "Forgery and Counterfeiting",
                            Crime >= 110000 && Crime < 120000 ~ "Fraud",
                            Crime >= 120000 && Crime < 130000 ~ "Embezzlement",
                            Crime >= 130000 && Crime < 140000 ~ "Stolen Property: Buying, Receiving, Possesing",
                            Crime >= 140000 && Crime < 150000 ~ "Vandalism",
                            Crime >= 150000 && Crime < 160000 ~ "Weapons: Carrying, Possessing, etc",
                            Crime >= 160000 && Crime < 170000 ~ "Prostitution and Commercialized Vice",
                            Crime >= 170000 && Crime < 180000 ~ "Sex Offenses",
                            Crime >= 180000 && Crime < 190000 ~ "Drug Abuse Violations",
                            Crime >= 190000 && Crime < 200000 ~ "Gambling",
                            Crime >= 200000 && Crime < 210000 ~ "Offenses Against the Family and Children",
                            Crime >= 210000 && Crime < 220000 ~ "Liquor Laws",
                            Crime >= 220000 && Crime < 230000 ~ "Drunkeness",
                            Crime >= 230000 && Crime < 240000 ~ "Disorderly Conduct",
                            Crime >= 240000 && Crime < 250000 ~ "Vagrancy",
                            Crime >= 250000 && Crime < 260000 ~ "All Other Offenses",
                            Crime >= 260000 && Crime < 270000 ~ "Suspicion",
                            Crime >= 270000 && Crime < 280000 ~ "Curfew and Loitering Laws-Persons under 18",
                            Crime >= 280000 && Crime < 290000 ~ "Runaways-Persons under 18"))

z <- y %>%
  mutate(Violent = ifelse(Crime >= 90000, "FALSE", "TRUE"))
