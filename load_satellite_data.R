# Function to load orbital element data
load_satellite_data <- function(filename, dates = TRUE){
  require(tidyverse)
  
  # Load data and turn the variable names into snake case using janitor
  data <- read_csv(filename) %>% 
    rename(date = ...1) %>% 
    janitor::clean_names() %>% 
    tibble()
  
  # If desired, convert the observation times to dates from datetimes (to simplify the problem)
  if(dates == TRUE){
    data <- data %>% 
      mutate(date = as.Date(date))
  }
  return(data)
}

# Function to load all data - with true manoeuvre times 
load_all_satellite_data <- function(sat_name, lookup, dates = TRUE){
  # sat_name needs to be the name in the orbital element files
  # lookup needs to be the satellite name and launch date lookup dataframe at the top of the main file
  # set date = FALSE if want to use datetimes
  
  # Require packages
  require(tidyverse)
  
  # Get the locations of the unpropagated and propagated elements for 
  filename <- paste("satellite_data/orbital_elements/unpropagated_elements_",sat_name,".csv", sep = "")
  prop_filename <- paste("satellite_data/orbital_elements/propagated_elements_",sat_name,".csv", sep = "")
  
  ## PART I : UNPROPAGATED ORBITAL ELEMENTS
  
  # Run the above function to load the orbital element data
  data <- load_satellite_data(filename, dates = dates)
  
  # Get the launch date from the lookup dataframe
  launch_date <- lookup[lookup$sat_name == sat_name,]$launch_date
  
  # Remove the six months of data after the launch date
  data <- data %>% 
    filter(date >= launch_date + months(6))
  
  ## PART II : TRUE MANOEUVRE TIMES
  
  # Set the location of the manoeuvres data -- CHANGE if this changes
  manouevres_filename <- "cleaned_manoeuvre_times_2023-10-16.csv"
  
  # Load the manoeuvres for the given satellite
  man_dates <- read_csv(manouevres_filename) %>% 
    filter(name == lookup[lookup$sat_name == sat_name,]$man_data_name) %>% 
    mutate(
      interval = interval(start = begin_date, end = end_date),
      diff = int_length(interval),
      median_date = begin_date + (diff/2)
    ) %>% 
    select(name, median_date)
  
  # Convert to datetimes if that is what we are doing
  if(dates == TRUE){
    man_dates <- man_dates %>% 
      mutate(
        median_date = as.Date(median_date) 
        )
  }
  
  # Remove manoeuvres within 6 months of the launch date
  man_dates <- man_dates %>% 
    filter(median_date >= launch_date + months(6))
  
  # Merge manoeuvres into data frame ## IF I DECIDE TO CHANGE HOW MISSING DATES ARE CODED, CHANGE IT HERE !!!
  data <- data %>% 
    mutate(
      manoeuvre = if_else(date %in% man_dates$median_date, true = 1, false = 0)
    )
  
  # TROUBLE: WHAT ABOUT THE FACT THAT SOME MANOEUVRES OCCUR ON DATES NOT COVERED BY THE DATA?
  ## OR ...
  # man_dates <- man_dates %>% mutate(date = median_date, manoeuvre = 1) %>% select(date, manoeuvre)
  # full_join(data, man_dates, by = c("date")) %>% arrange(date)
  
  ## PART III : PROPAGATED VALUES
  
  # Add the propagated values to the data frame
  propagated <- load_satellite_data(prop_filename, dates = TRUE)
  data <- inner_join(data, propagated, by = c("date"), suffix = c("","_propagated"))
  
  # Return the dataframe
  return(data)
}


# Function to calculate the differences with the propagated values
get_propagated_differences <- function(data){
  # Must take as input a data frame created by the load_all_satellite_data function
  # Otherwise the variable names might not be correct
    
  # Require packages
  require(tidyverse)
  
  # Subtract the propagated values from the unpropagated values, save these as the observations for each orbital element
  data <- data %>%
    mutate(
      eccentricity = (eccentricity - eccentricity_propagated),
      argument_of_perigee = (argument_of_perigee - argument_of_perigee_propagated),
      mean_anomaly = (mean_anomaly - mean_anomaly_propagated),
      brouwer_mean_motion = (brouwer_mean_motion - brouwer_mean_motion_propagated),
      right_ascension = (right_ascension - right_ascension_propagated)
    ) %>% 
    select(date, eccentricity, argument_of_perigee, inclination, 
         mean_anomaly, brouwer_mean_motion, right_ascension, manoeuvre)
  
  # Return the transformed dataframe
  return(data)
}
