# This is the code for the report.
# Running the whole file should generate all of the figures.

# ---- SETUP -----

# Load packages
pacman::p_load(tidyverse, patchwork, janitor, forecast, yardstick, caret, isotree)

# Need a satellite lookup data frame, containing for each satellite
# - its name
# - its name in the maneouvre times file
# - the name of the file where it is stored
# - the launch date (we want to exclude a certain amount of data immediately 
#     after the launch date, as it is not reliable )

lookup <- data.frame(
  # sat_name must be the same as the orbital elements data file name
  sat_name = c("CryoSat-2", "Fengyun-2D", "Fengyun-2E", "Fengyun-2F",
               "Fengyun-2H", "Fengyun-4A", "Haiyang-2A", "Jason-1",
               "Jason-2", "Jason-3", "SARAL", "Sentinel-3A",
               "Sentinel-3B", "Sentinel-6A", "TOPEX"),
  # We also need the name by which the satellite appears in the manoeuvres data
  man_data_name = c("CRYO2","FY2D","FY2E","FY2F",
                    "FY2H","FY4A","HY-2A","JASO1",
                    "JASO2","JASO3","SARAL","SEN3A",
                    "SEN3B","SEN6A","TOPEX"),
  # Note: as.Date date format is Year/Month/Day (a sensible format)
  # Note: the dates were sourced from wikipedia
  launch_date = as.Date(c("2010/04/08", "2006/12/08", "2008/12/23", "2012/01/13",
                          "2018/06/05", "2016/12/10", "2011/08/15", "2001/12/07",
                          "2008/06/20", "2016/01/17", "2013/02/25", "2016/02/16",
                          "2018/04/25", "2020/11/21", "1992/08/10"))
)

# Load the functions we have written
source(here::here("load_satellite_data.R"))
source(here::here("plot_satellite_data.R"))
source(here::here("anomaly_detection.R"))
source(here::here("evaluation.R"))


# ----- LOAD DATA -----
# We load the data and calculate the differences with the propagated values

# CryoSat-2
data_cryosat2 <- load_all_satellite_data("CryoSat-2", lookup = lookup, dates = TRUE)
diffs_cryosat2 <- get_propagated_differences(data_cryosat2)

# Fengyun-2D
data_fengyun2d <- load_all_satellite_data("Fengyun-2D", lookup = lookup, dates = TRUE)
diffs_fengyun2d <- get_propagated_differences(data_fengyun2d)

# Jason-1
data_jason1 <- load_all_satellite_data("Jason-1", lookup = lookup, dates = TRUE)
diffs_jason1 <- get_propagated_differences(data_jason1)

# Sentinel-3A
data_sentinel3a <- load_all_satellite_data("Sentinel-3A", lookup = lookup, dates = TRUE)
diffs_sentinel3a <- get_propagated_differences(data_sentinel3a)


# ----- EXPLORATORY DATA ANALYSIS -----
# For each satellite, we plot the orbital elements

# CryoSat-2 observations (unpropagated)
plot_satellite_data(data_cryosat2, date_range = as.Date(c("2015-01-01", "2018-01-01")))
ggsave(filename = "figs/eda_unprop_cryosat2.png", width = 9, height = 5.5)

# Fengyun-2D observations (unpropagated)
plot_satellite_data(data_fengyun2d, date_range = as.Date(c("2011-01-01", "2015-01-01")))
ggsave(filename = "figs/eda_unprop_fengyun2d.png", width = 9, height = 5.5)

# Jason-1 observations (unpropagated)
plot_satellite_data(data_jason1, date_range = as.Date(c("2004-01-01", "2008-01-01")))
ggsave(filename = "figs/eda_unprop_jason1.png", width = 9, height = 5.5)

# Sentinel-3A observations (unpropagated)
plot_satellite_data(data_sentinel3a, date_range = as.Date(c("2018-01-01", "2022-01-01")))
ggsave(filename = "figs/eda_unprop_sentinel3a.png", width = 9, height = 5.5)


# CryoSat-2 differences with propagations (unpropagated - propagated)
diffs_cryosat2 %>% 
  filter(date != "2017-06-15") %>% # anomaly which totally obscures the pattern
  filter(date != "2017-06-16") %>% # anomaly which totally obscures the pattern
  plot_satellite_data_diffs(date_range = as.Date(c("2015-01-01", "2018-01-01")))
ggsave(filename = "figs/eda_diffs_cryosat2.png", width = 9, height = 5.5)

# Fengyun-2D differences with propagations (unpropagated - propagated)
plot_satellite_data_diffs(diffs_fengyun2d, date_range = as.Date(c("2011-01-01", "2015-01-01")))
ggsave(filename = "figs/eda_diffs_fengyun2d.png", width = 9, height = 5.5)

# Jason-1 differences with propagations (unpropagated - propagated)
plot_satellite_data_diffs(diffs_jason1, date_range = as.Date(c("2004-01-01", "2008-01-01")))
ggsave(filename = "figs/eda_diffs_jason1.png", width = 9, height = 5.5)

# Sentinel-3A differences with propagations (unpropagated - propagated)
plot_satellite_data_diffs(diffs_sentinel3a, date_range = as.Date(c("2018-01-01", "2022-01-01")))
ggsave(filename = "figs/eda_diffs_sentinel3a.png", width = 9, height = 5.5)


# Closer up look for how wide to make the moving window
plot_satellite_data(diffs_cryosat2, date_range = as.Date(c("2015-11-01","2015-11-07")))
plot_satellite_data(diffs_cryosat2, date_range = as.Date(c("2011-11-29", "2011-12-04"))) # there is a manoeuvre here that doesn't show up due to missing data
plot_satellite_data(diffs_sentinel3a, date_range = as.Date(c("2021-08-08","2021-08-14")))

# Brouwer Mean Motion: if there, anomalies turn up either on the period or the next one after


# ----- DERIVE THRESHOLDS ------
# We calculate the threshold to be the inverse of the proportion of manoeuvres in the data.

threshold_cryosat2 <- 1 - (data_cryosat2 %>% filter(manoeuvre == 1) %>% nrow())/nrow(data_cryosat2)
threshold_fengyun2d <- 1 - (data_fengyun2d %>% filter(manoeuvre == 1) %>% nrow())/nrow(data_fengyun2d)
threshold_jason1 <- 1 - (data_jason1 %>% filter(manoeuvre == 1) %>% nrow())/nrow(data_jason1)
threshold_sentinel3a <- 1 - (data_sentinel3a %>% filter(manoeuvre == 1) %>% nrow())/nrow(data_sentinel3a)


# ----- BASELINE METHOD -----
# apply to each orbital element of each satellite

### CryoSat-2 ###

# Eccentricity
base_cryosat2_ecc <- baseline_anomalies(diffs_cryosat2, "eccentricity", threshold_cryosat2)
base_cryosat2_ecc <- evaluate_moving_window(base_cryosat2_ecc, d = 1)
get_metrics(base_cryosat2_ecc)
plot_pr_curve(base_cryosat2_ecc)
plot_roc_curve(base_cryosat2_ecc)

# Argument of Perigee
base_cryosat2_aop <- baseline_anomalies(diffs_cryosat2, "argument_of_perigee", threshold_cryosat2)
base_cryosat2_aop <- evaluate_moving_window(base_cryosat2_aop, d = 1)
get_metrics(base_cryosat2_aop)
plot_pr_curve(base_cryosat2_aop)
plot_roc_curve(base_cryosat2_aop)

# Brouwer Mean Motion
base_cryosat2_bmm <- baseline_anomalies(diffs_cryosat2, "brouwer_mean_motion", threshold_cryosat2)
base_cryosat2_bmm <- evaluate_moving_window(base_cryosat2_bmm, d = 1)
get_metrics(base_cryosat2_bmm)
plot_pr_curve(base_cryosat2_bmm)
ggsave("figs/baseline_bmm_pr_cryosat2.png") # save this figure for the report
plot_roc_curve(base_cryosat2_bmm)
ggsave("figs/baseline_bmm_roc_cryosat2.png") # save this figure for the report


# Plot predictions of one of the methods
p1 <- plot_predictions(base_cryosat2_ecc) + 
  theme(legend.position = c(.1,.9)) +
  labs(y = "Difference",
       title = "Eccentricity")
p2 <- plot_predictions(base_cryosat2_aop) + 
  theme(legend.position = c(.1,.9)) +
  labs(y = "Difference",
       title = "Argument of Perigee")
p3 <- plot_predictions(base_cryosat2_bmm) + 
  theme(legend.position = c(.1,.9)) +
  labs(y = "Difference",
       title = "Brouwer Mean Motion")
p <- p1 / p2 / p3 & theme(legend.position = "bottom")
p + plot_layout(guides = "collect")
ggsave("figs/baseline_bmm_cryosat2_predictions.png", width = 7.7, height = 6.66)


### Fengyun 2D ###

# Eccentricity
base_fengyun2d_ecc <- baseline_anomalies(diffs_fengyun2d, "eccentricity", threshold_fengyun2d)
base_fengyun2d_ecc <- evaluate_moving_window(base_fengyun2d_ecc, d = 1)
get_metrics(base_fengyun2d_ecc)
plot_pr_curve(base_fengyun2d_ecc)
ggsave("figs/baseline_pr_fengyun2d_ecc.png")
plot_roc_curve(base_fengyun2d_ecc)

# Argument of Perigee
base_fengyun2d_aop <- baseline_anomalies(diffs_fengyun2d, "argument_of_perigee", threshold_fengyun2d)
base_fengyun2d_aop <- evaluate_moving_window(base_fengyun2d_aop, d = 1)
get_metrics(base_fengyun2d_aop)
plot_pr_curve(base_fengyun2d_aop)
plot_roc_curve(base_fengyun2d_aop)

# Brouwer Mean Motion
base_fengyun2d_bmm <- baseline_anomalies(diffs_fengyun2d, "brouwer_mean_motion", threshold_fengyun2d)
base_fengyun2d_bmm <- evaluate_moving_window(base_fengyun2d_bmm, d = 1)
get_metrics(base_fengyun2d_bmm)
plot_pr_curve(base_fengyun2d_bmm)
ggsave("figs/baseline_bmm_pr_fengyun2d.png")
plot_roc_curve(base_fengyun2d_bmm)


### Jason-1 ###

# Eccentricity
base_jason1_ecc <- baseline_anomalies(diffs_jason1, "eccentricity", threshold_jason1)
base_jason1_ecc <- evaluate_moving_window(base_jason1_ecc, d = 1)
get_metrics(base_jason1_ecc)
plot_pr_curve(base_jason1_ecc)
plot_roc_curve(base_jason1_ecc)

# Argument of Perigee
base_jason1_aop <- baseline_anomalies(diffs_jason1, "argument_of_perigee", threshold_jason1)
base_jason1_aop <- evaluate_moving_window(base_jason1_aop, d = 1)
get_metrics(base_jason1_aop)
plot_pr_curve(base_jason1_aop)
plot_roc_curve(base_jason1_aop)

# Brouwer Mean Motion
base_jason1_bmm <- baseline_anomalies(diffs_jason1, "brouwer_mean_motion", threshold_jason1)
base_jason1_bmm <- evaluate_moving_window(base_jason1_bmm, d = 1)
get_metrics(base_jason1_bmm)
plot_pr_curve(base_jason1_bmm)
plot_roc_curve(base_jason1_bmm)


### Sentinel-3A ###

# Eccentricity
base_sentinel3a_ecc <- baseline_anomalies(diffs_sentinel3a, "eccentricity", threshold_sentinel3a)
base_sentinel3a_ecc <- evaluate_moving_window(base_sentinel3a_ecc, d = 1)
get_metrics(base_sentinel3a_ecc)
plot_pr_curve(base_sentinel3a_ecc)
plot_roc_curve(base_sentinel3a_ecc)

# Argument of Perigee
base_sentinel3a_aop <- baseline_anomalies(diffs_sentinel3a, "argument_of_perigee", threshold_sentinel3a)
base_sentinel3a_aop <- evaluate_moving_window(base_sentinel3a_aop, d = 1)
get_metrics(base_sentinel3a_aop)
plot_pr_curve(base_sentinel3a_aop)
plot_roc_curve(base_sentinel3a_aop) # very bad

# Brouwer Mean Motion
base_sentinel3a_bmm <- baseline_anomalies(diffs_sentinel3a, "brouwer_mean_motion", threshold_sentinel3a)
base_sentinel3a_bmm <- evaluate_moving_window(base_sentinel3a_bmm, d = 1)
get_metrics(base_sentinel3a_bmm)
plot_pr_curve(base_sentinel3a_bmm) # definitely a better threshold. this looks amazings
ggsave("figs/baseline_bmm_pr_sentinel3a.png")
plot_roc_curve(base_sentinel3a_bmm)




# ----- ARIMA on UNPROPAGATED (Brouwer Mean Motion) -----

### CryoSat-2 ###
# Brouwer Mean Motion
arima_unprop_cryosat2_bmm <- arima_anomalies(data_cryosat2, "brouwer_mean_motion", threshold_cryosat2)
arima_unprop_cryosat2_bmm <- evaluate_moving_window(arima_unprop_cryosat2_bmm, d = 1)
get_metrics(arima_unprop_cryosat2_bmm)
plot_pr_curve(arima_unprop_cryosat2_bmm) # there is a better threshold with higher recall
plot_roc_curve(arima_unprop_cryosat2_bmm)

### Fengyun 2D ###
# Brouwer Mean Motion
arima_unprop_fengyun2d_bmm <- arima_anomalies(data_fengyun2d, "brouwer_mean_motion", threshold_fengyun2d)
arima_unprop_fengyun2d_bmm <- evaluate_moving_window(arima_unprop_fengyun2d_bmm, d = 1)
get_metrics(arima_unprop_fengyun2d_bmm)
plot_pr_curve(arima_unprop_fengyun2d_bmm)
ggsave("figs/arima_raw_pr_fengyun2d.png") # save this figure for the report
plot_roc_curve(arima_unprop_fengyun2d_bmm)

### Jason-1 ###
# Brouwer Mean Motion
arima_unprop_jason1_bmm <- arima_anomalies(data_jason1, "brouwer_mean_motion", threshold_jason1)
arima_unprop_jason1_bmm <- evaluate_moving_window(arima_unprop_jason1_bmm, d = 1)
get_metrics(arima_unprop_jason1_bmm)
plot_pr_curve(arima_unprop_jason1_bmm) # possible better option?
plot_roc_curve(arima_unprop_jason1_bmm)

### Sentinel-3A ###
# Brouwer Mean Motion
arima_unprop_sentinel3a_bmm <- arima_anomalies(data_sentinel3a, "brouwer_mean_motion", threshold_sentinel3a)
arima_unprop_sentinel3a_bmm <- evaluate_moving_window(arima_unprop_sentinel3a_bmm, d = 1)
get_metrics(arima_unprop_sentinel3a_bmm)
plot_pr_curve(arima_unprop_sentinel3a_bmm)
plot_roc_curve(arima_unprop_sentinel3a_bmm)




# ----- ARIMA on DIFFERENCES -----

### CryoSat-2 ###

# Eccentricity
arima_cryosat2_ecc <- arima_anomalies(diffs_cryosat2, "eccentricity", threshold_cryosat2)
arima_cryosat2_ecc <- evaluate_moving_window(arima_cryosat2_ecc, d = 1)
get_metrics(arima_cryosat2_ecc)
plot_pr_curve(arima_cryosat2_ecc)
ggsave("figs/arima_ecc_cryosat2_pr.png")
plot_roc_curve(arima_cryosat2_ecc)

# Argument of Perigee
arima_cryosat2_aop <- arima_anomalies(diffs_cryosat2, "argument_of_perigee", threshold_cryosat2)
arima_cryosat2_aop <- evaluate_moving_window(arima_cryosat2_aop, d = 1)
get_metrics(arima_cryosat2_aop)
plot_pr_curve(arima_cryosat2_aop)
plot_roc_curve(arima_cryosat2_aop)

# Brouwer Mean Motion
arima_cryosat2_bmm <- arima_anomalies(diffs_cryosat2, "brouwer_mean_motion", threshold_cryosat2)
arima_cryosat2_bmm <- evaluate_moving_window(arima_cryosat2_bmm, d = 1)
get_metrics(arima_cryosat2_bmm)
plot_pr_curve(arima_cryosat2_bmm) # okay
plot_roc_curve(arima_cryosat2_bmm)

# Plot predictions of one of the methods
p1 <- plot_predictions(arima_cryosat2_ecc) + 
  labs(y = "Difference",
       title = "Eccentricity")
p2 <- plot_predictions(arima_cryosat2_aop) + 
  labs(y = "Difference",
       title = "Argument of Perigee")
p3 <- plot_predictions(arima_cryosat2_bmm) + 
  labs(y = "Difference",
       title = "Brouwer Mean Motion")
p <- p1 / p2 / p3 & theme(legend.position = "bottom")
p + plot_layout(guides = "collect")
ggsave("figs/arima_cryosat2_predictions.png", width = 7.7, height = 6.66)




### Fengyun 2D ###

# Eccentricity
arima_fengyun2d_ecc <- arima_anomalies(diffs_fengyun2d, "eccentricity", threshold_fengyun2d)
arima_fengyun2d_ecc <- evaluate_moving_window(arima_fengyun2d_ecc, d = 1)
get_metrics(arima_fengyun2d_ecc)
plot_pr_curve(arima_fengyun2d_ecc) # there is a better threshold
plot_roc_curve(arima_fengyun2d_ecc) # good

# Argument of Perigee
arima_fengyun2d_aop <- arima_anomalies(diffs_fengyun2d, "argument_of_perigee", threshold_fengyun2d)
arima_fengyun2d_aop <- evaluate_moving_window(arima_fengyun2d_aop, d = 1)
get_metrics(arima_fengyun2d_aop)
plot_pr_curve(arima_fengyun2d_aop)
plot_roc_curve(arima_fengyun2d_aop)

# Brouwer Mean Motion
arima_fengyun2d_bmm <- arima_anomalies(diffs_fengyun2d, "brouwer_mean_motion", threshold_fengyun2d)
arima_fengyun2d_bmm <- evaluate_moving_window(arima_fengyun2d_bmm, d = 1)
get_metrics(arima_fengyun2d_bmm)
plot_pr_curve(arima_fengyun2d_bmm) # potential better threshold
plot_roc_curve(arima_fengyun2d_bmm)


### Jason-1 ###

# Eccentricity
arima_jason1_ecc <- arima_anomalies(diffs_jason1, "eccentricity", threshold_jason1)
arima_jason1_ecc <- evaluate_moving_window(arima_jason1_ecc, d = 1)
get_metrics(arima_jason1_ecc)
plot_pr_curve(arima_jason1_ecc)
plot_roc_curve(arima_jason1_ecc)

# Argument of Perigee
arima_jason1_aop <- arima_anomalies(diffs_jason1, "argument_of_perigee", threshold_jason1)
arima_jason1_aop <- evaluate_moving_window(arima_jason1_aop, d = 1)
get_metrics(arima_jason1_aop)
plot_pr_curve(arima_jason1_aop)
plot_roc_curve(arima_jason1_aop)

# Brouwer Mean Motion
arima_jason1_bmm <- arima_anomalies(diffs_jason1, "brouwer_mean_motion", threshold_jason1)
arima_jason1_bmm <- evaluate_moving_window(arima_jason1_bmm, d = 1)
get_metrics(arima_jason1_bmm)
plot_pr_curve(arima_jason1_bmm)
plot_roc_curve(arima_jason1_bmm)


### Sentinel-3A ###

# Eccentricity
arima_sentinel3a_ecc <- arima_anomalies(diffs_sentinel3a, "eccentricity", threshold_sentinel3a)
arima_sentinel3a_ecc <- evaluate_moving_window(arima_sentinel3a_ecc, d = 1)
get_metrics(arima_sentinel3a_ecc)
plot_pr_curve(arima_sentinel3a_ecc)
plot_roc_curve(arima_sentinel3a_ecc)

# Argument of Perigee
arima_sentinel3a_aop <- arima_anomalies(diffs_sentinel3a, "argument_of_perigee", threshold_sentinel3a)
arima_sentinel3a_aop <- evaluate_moving_window(arima_sentinel3a_aop, d = 1)
get_metrics(arima_sentinel3a_aop)
plot_pr_curve(arima_sentinel3a_aop)
plot_roc_curve(arima_sentinel3a_aop)

# Brouwer Mean Motion
arima_sentinel3a_bmm <- arima_anomalies(diffs_sentinel3a, "brouwer_mean_motion", threshold_sentinel3a)
arima_sentinel3a_bmm <- evaluate_moving_window(arima_sentinel3a_bmm, d = 1)
get_metrics(arima_sentinel3a_bmm)
plot_pr_curve(arima_sentinel3a_bmm) # very good; definite better threshold
plot_roc_curve(arima_sentinel3a_bmm)


# ----- Isolation Forest on Differences (individual variables) -----

### CryoSat-2 ###

# Eccentricity
if_cryosat2_ecc <- isolation_forest_anomalies_univariate(diffs_cryosat2, "eccentricity", threshold_cryosat2)
if_cryosat2_ecc <- evaluate_moving_window(if_cryosat2_ecc, d = 1)
get_metrics(if_cryosat2_ecc)
plot_pr_curve(if_cryosat2_ecc)
plot_roc_curve(if_cryosat2_ecc)

# Argument of Perigee
if_cryosat2_aop <- isolation_forest_anomalies_univariate(diffs_cryosat2, "argument_of_perigee", threshold_cryosat2)
if_cryosat2_aop <- evaluate_moving_window(if_cryosat2_aop, d = 1)
get_metrics(if_cryosat2_aop)
plot_pr_curve(if_cryosat2_aop)
plot_roc_curve(if_cryosat2_aop)

# Brouwer Mean Motion
if_cryosat2_bmm <- isolation_forest_anomalies_univariate(diffs_cryosat2, "brouwer_mean_motion", threshold_cryosat2)
if_cryosat2_bmm <- evaluate_moving_window(if_cryosat2_bmm, d = 1)
get_metrics(if_cryosat2_bmm)
plot_pr_curve(if_cryosat2_bmm) # there is a sort-of better threshold
plot_roc_curve(if_cryosat2_bmm)

# Plot predictions of one of the methods
p1 <- plot_predictions(if_cryosat2_ecc) + 
  labs(y = "Difference",
       title = "Eccentricity")
p2 <- plot_predictions(if_cryosat2_aop) + 
  labs(y = "Difference",
       title = "Argument of Perigee")
p3 <- plot_predictions(if_cryosat2_bmm) + 
  labs(y = "Difference",
       title = "Brouwer Mean Motion")
p <- p1 / p2 / p3 & theme(legend.position = "bottom")
p + plot_layout(guides = "collect")
ggsave("figs/if_cryosat2_predictions.png", width = 7.7, height = 6.66)


### Fengyun 2D ###

# Eccentricity
if_fengyun2d_ecc <- isolation_forest_anomalies_univariate(diffs_fengyun2d, "eccentricity", threshold_fengyun2d)
if_fengyun2d_ecc <- evaluate_moving_window(if_fengyun2d_ecc, d = 1)
get_metrics(if_fengyun2d_ecc)
plot_pr_curve(if_fengyun2d_ecc) # okay
ggsave("figs/if_pr_fengyun2d_ecc.png")
plot_roc_curve(if_fengyun2d_ecc)

# Argument of Perigee
if_fengyun2d_aop <- isolation_forest_anomalies_univariate(diffs_fengyun2d, "argument_of_perigee", threshold_fengyun2d)
if_fengyun2d_aop <- evaluate_moving_window(if_fengyun2d_aop, d = 1)
get_metrics(if_fengyun2d_aop)
plot_pr_curve(if_fengyun2d_aop)
plot_roc_curve(if_fengyun2d_aop)

# Brouwer Mean Motion
if_fengyun2d_bmm <- isolation_forest_anomalies_univariate(diffs_fengyun2d, "brouwer_mean_motion", threshold_fengyun2d)
if_fengyun2d_bmm <- evaluate_moving_window(if_fengyun2d_bmm, d = 1)
get_metrics(if_fengyun2d_bmm)
plot_pr_curve(if_fengyun2d_bmm) # definite better threshold
ggsave("figs/if_pr_fengyun2d_bmm.png")
plot_roc_curve(if_fengyun2d_bmm)

### Jason-1 ###

# Eccentricity
if_jason1_ecc <- isolation_forest_anomalies_univariate(diffs_jason1, "eccentricity", threshold_jason1)
if_jason1_ecc <- evaluate_moving_window(if_jason1_ecc, d = 1)
get_metrics(if_jason1_ecc)
plot_pr_curve(if_jason1_ecc)
plot_roc_curve(if_jason1_ecc)

# Argument of Perigee
if_jason1_aop <- isolation_forest_anomalies_univariate(diffs_jason1, "argument_of_perigee", threshold_jason1)
if_jason1_aop <- evaluate_moving_window(if_jason1_aop, d = 1)
get_metrics(if_jason1_aop)
plot_pr_curve(if_jason1_aop)
plot_roc_curve(if_jason1_aop)

# Brouwer Mean Motion
if_jason1_bmm <- isolation_forest_anomalies_univariate(diffs_jason1, "brouwer_mean_motion", threshold_jason1)
if_jason1_bmm <- evaluate_moving_window(if_jason1_bmm, d = 1)
get_metrics(if_jason1_bmm)
plot_pr_curve(if_jason1_bmm)
plot_roc_curve(if_jason1_bmm)


### Sentinel-3A ###

# Eccentricity
if_sentinel3a_ecc <- isolation_forest_anomalies_univariate(diffs_sentinel3a, "eccentricity", threshold_sentinel3a)
if_sentinel3a_ecc <- evaluate_moving_window(if_sentinel3a_ecc, d = 1)
get_metrics(if_sentinel3a_ecc)
plot_pr_curve(if_sentinel3a_ecc)
plot_roc_curve(if_sentinel3a_ecc)

# Argument of Perigee
if_sentinel3a_aop <- isolation_forest_anomalies_univariate(diffs_sentinel3a, "argument_of_perigee", threshold_sentinel3a)
if_sentinel3a_aop <- evaluate_moving_window(if_sentinel3a_aop, d = 1)
get_metrics(if_sentinel3a_aop)
plot_pr_curve(if_sentinel3a_aop)
plot_roc_curve(if_sentinel3a_aop)

# Brouwer Mean Motion
if_sentinel3a_bmm <- isolation_forest_anomalies_univariate(diffs_sentinel3a, "brouwer_mean_motion", threshold_sentinel3a)
if_sentinel3a_bmm <- evaluate_moving_window(if_sentinel3a_bmm, d = 1)
get_metrics(if_sentinel3a_bmm)
plot_pr_curve(if_sentinel3a_bmm)
ggsave("figs/if_pr_sentinel3a_bmm.png")
plot_roc_curve(if_sentinel3a_bmm)




# ----- Isolation Forest on Differences (multivariate) -----
# Set which variables we are using
if_multivar_vars <- c("brouwer_mean_motion", "eccentricity", "argument_of_perigee")

# CryoSat-2
if_cryosat2 <- isolation_forest_anomalies_multivariate(diffs_cryosat2, if_multivar_vars, threshold_cryosat2)
if_cryosat2 <- evaluate_moving_window(if_cryosat2, d = 1)
get_metrics(if_cryosat2)
plot_pr_curve(if_cryosat2)
plot_roc_curve(if_cryosat2)

# Fengyun 2D
if_fengyun2d <- isolation_forest_anomalies_multivariate(diffs_fengyun2d, if_multivar_vars, threshold_fengyun2d)
if_fengyun2d <- evaluate_moving_window(if_fengyun2d, d = 1)
get_metrics(if_fengyun2d)
plot_pr_curve(if_fengyun2d)
plot_roc_curve(if_fengyun2d)

# Jason 1
if_jason1 <- isolation_forest_anomalies_multivariate(diffs_jason1, if_multivar_vars, threshold_jason1)
if_jason1 <- evaluate_moving_window(if_jason1, d = 1)
get_metrics(if_jason1)
plot_pr_curve(if_jason1)
plot_roc_curve(if_jason1)

# Sentinel 3A
if_sentinel3a <- isolation_forest_anomalies_multivariate(diffs_sentinel3a, if_multivar_vars, threshold_sentinel3a)
if_sentinel3a <- evaluate_moving_window(if_sentinel3a, d = 1)
get_metrics(if_sentinel3a)
plot_pr_curve(if_sentinel3a) # much better threshold
plot_roc_curve(if_sentinel3a)



