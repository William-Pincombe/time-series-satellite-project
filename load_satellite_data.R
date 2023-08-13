# Function to load satellite data
load_satellite_data <- function(filename){
  require(tidyverse)
  data <- read_csv(filename) %>% 
    rename(date = ...1) %>% 
    janitor::clean_names()
  return(data)
}
