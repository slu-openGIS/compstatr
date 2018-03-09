#'
cs_load_year <- function(path){
  
  files <- dir(path = path, pattern = "*.csv")
  
  data <- files %>% 
    map(~ suppressMessages(suppressWarnings(read_csv(file.path(path, .)))))
  
}
