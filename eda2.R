# More advanced - plotting different data sources using functions

# Load packages
pacman::p_load(tidyverse, patchwork)

# PLOTTING UNPROPAGATED DATA FOR DIFFERENT SATELLITES

# Cryosat2
Cryosat2 <- load_satellite_data("satellite_data/orbital_elements/unpropagated_elements_CryoSat-2.csv")
plot_satellite_data(cryosat2)

# Fengyun-2D
Fengyun2D <- load_satellite_data("satellite_data/orbital_elements/unpropagated_elements_Fengyun-2D.csv")
plot_satellite_data(fengyun2D)

# Fengyun-2E
Fengyun2E <- load_satellite_data("satellite_data/orbital_elements/unpropagated_elements_Fengyun-2E.csv")
plot_satellite_data(fengyun2E)

# jason-1
Jason1 <- load_satellite_data("satellite_data/orbital_elements/unpropagated_elements_Jason-1.csv")
plot_satellite_data(jason1)

# sentinel-3A
Sentinel3A <- load_satellite_data("satellite_data/orbital_elements/unpropagated_elements_Sentinel-3A.csv")
plot_satellite_data(sentinel3A)


# COMPARING TO TRUE VALUES
# START WITH FENGYUN AS THEY HAVE ANOMALY TIMES STORED IN BETTER FORMAT

Fengyun2E <- load_satellite_data(here::here("satellite_data","orbital_elements","unpropagated_elements_Fengyun-2E.csv")) # loaded above

# Import anomaly data
Fengyun2E_anomalies <- read_delim(here::here("satellite_data","manoeuvres","manFY2E.txt.fy"), col_names = FALSE)

# Clean; select begin and end times of anomalies
Fengyun2E_anomalies <- Fengyun2E_anomalies %>% 
  select(X3,X4) %>% 
  rename(begin = X3, end = X4) %>% 
  mutate(
    begin = as_datetime(begin),
    end = as_datetime(end)
  )
Fengyun2E_anomalies

ggplot(aes(x = date, y = mean_anomaly), data = Fengyun2E) + 
  geom_vline(data = Fengyun2E_anomalies, aes(xintercept = begin), linetype = 2) + 
  geom_line(colour = 'red') +
  xlim(as_date("2013-01-01"),as_date("2014-01-01"))
  # ylim(0,0.001)

# Compare to propagated data; plot differences
Fengyun2E_prop <- load_satellite_data(here::here("satellite_data","orbital_elements","propagated_elements_Fengyun-2E.csv")) # loaded above
merge(Fengyun2E, Fengyun2E_prop, by = c("date")) %>% 
  mutate(
    eccentricity = eccentricity.x - eccentricity.y,
    argument_of_perigee = argument_of_perigee.x - argument_of_perigee.y,
    inclination = inclination.x - inclination.y,
    mean_anomaly = mean_anomaly.x - mean_anomaly.y,
    brouwer_mean_motion = brouwer_mean_motion.x - brouwer_mean_motion.y,
    right_ascension = right_ascension.x - right_ascension.y
  ) %>% 
  select(date, eccentricity, argument_of_perigee, inclination, 
         mean_anomaly, brouwer_mean_motion, right_ascension) %>% 
  # plot_satellite_data()
ggplot(aes(x = date, y = eccentricity)) + 
  geom_vline(data = Fengyun2E_anomalies, aes(xintercept = begin), linetype = 2) + 
  geom_line(colour = 'red') + 
  ylim(-0.001,0.001)

# Look at first differences 
ggplot(data = Fengyun2E, aes(x = date, y = (eccentricity - lag(eccentricity)))) + 
  geom_line() + 
  ylim(-0.001,0.001)




# What about Cryosat2
Cryosat2 <- load_satellite_data(here::here("satellite_data","orbital_elements","unpropagated_elements_CryoSat-2.csv"))
Cryosat2_prop <- load_satellite_data(here::here("satellite_data","orbital_elements","propagated_elements_CryoSat-2.csv"))
merge(Fengyun2E, Fengyun2E_prop, by = c("date")) %>% 
  mutate(
    eccentricity = eccentricity.x - eccentricity.y,
    argument_of_perigee = argument_of_perigee.x - argument_of_perigee.y,
    inclination = inclination.x - inclination.y,
    mean_anomaly = mean_anomaly.x - mean_anomaly.y,
    brouwer_mean_motion = brouwer_mean_motion.x - brouwer_mean_motion.y,
    right_ascension = right_ascension.x - right_ascension.y
  ) %>% 
  select(date, eccentricity, argument_of_perigee, inclination, 
         mean_anomaly, brouwer_mean_motion, right_ascension) %>% 
  plot_satellite_data()


