###################################
## CALCULATING POINT PREDICTIONS                         - OLD FILE!
## FOR BASELINE PROPAGATED-DIFFERENCES METHOD
###################################

# Load packages
pacman::p_load(tidyverse)

# Load functions I have written
source("load_satellite_data.R")

# Load the data
data <- load_satellite_data(here::here("satellite_data","orbital_elements","unpropagated_elements_CryoSat-2.csv"))

# Load the propagated values
propagated <- load_satellite_data(here::here("satellite_data","orbital_elements","propagated_elements_CryoSat-2.csv"))



# Code to reduce datetimes to dates
data %>% 
  mutate(date = as.Date(date))





# Take the differences
differences <- merge(data, propagated, by = c("date")) %>% 
  mutate(
    eccentricity = (eccentricity.x - eccentricity.y),
    argument_of_perigee = (argument_of_perigee.x - argument_of_perigee.y),
    inclination = (inclination.x - inclination.y),
    mean_anomaly = (mean_anomaly.x - mean_anomaly.y),
    brouwer_mean_motion = (brouwer_mean_motion.x - brouwer_mean_motion.y),
    right_ascension = (right_ascension.x - right_ascension.y)
  ) %>% 
  select(date, eccentricity, argument_of_perigee, inclination, 
         mean_anomaly, brouwer_mean_motion, right_ascension)

# Select only the orbital element we're interested in
orbital_element <- "brouwer_mean_motion"
differences <- differences %>% 
  select(date, {{orbital_element}})

# Set the number of standard deviations above which we classify as anomalous
n_sd <- 2

# Calculate threshold based on standard deviation
threshold <- sd(differences[,2]) * n_sd

# Set anomaly point predictions where the difference is above the threshold
differences <- differences %>% 
  mutate(
    anomalous = abs(brouwer_mean_motion) > threshold
  )






# differences %>% 
#   ggplot(aes(x = brouwer_mean_motion)) + 
#   geom_histogram() + 
#   geom_vline(xintercept = sd(differences$brouwer_mean_motion))
# 
# sd(differences$brouwer_mean_motion)




