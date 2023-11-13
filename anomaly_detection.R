

# Function to normalise residuals (or differences), and then calculate a probability assuming a N(0,1) distribution
anomaly_score <- function(res){
  
  # take absolute value
  res <- abs(res)
  
  # calculate z-scores
  mu <- mean(res)
  sigma <- sd(res)
  
  z <- (res-mu)/sigma
  
  # hist(z)
  
  # calculate probability of anomaly
  anom_prob <- pnorm(z)
  
  return(anom_prob)
}

baseline_anomalies <- function(data, orbital_element, threshold){
  # `data` must be a data frame where the differences for each orbital element
  # have the variable name of that orbital element
  # (what is produced by applying get_propagated_differences() to loaded data)
  
  # Require packages
  require(tidyverse)
  
  # Return a data frame with the anomaly score based on the residuals
  result <- tibble(date = data$date,
                   observed = data[[orbital_element]],
                   manoeuvre = data$manoeuvre,
                   score = anomaly_score(observed)
                   )
  
  # Detect anomalies based on the threshold and the probability score
  anomalies <- result %>% 
    filter(score >= threshold)
  
  # Add predictions tot he data frame
  result <- result %>% 
    mutate(predict_anomaly = ifelse(date %in% anomalies$date, 1, 0))
  
  return(result)
}

# ASSUMPTION CHECKING: (it seems the assumption of normality is probably not met very well)

# # Select only the orbital element we're interested in
# orbital_element <- "brouwer_mean_motion"
# differences <- differences %>% 
#   select(date, {{orbital_element}}, manoeuvre)
# 
# # What is the distribution of the propagated residuals?. We want them to be normal
# hist(differences$brouwer_mean_motion)
# # Normal QQ-plot:
# qqnorm(differences$brouwer_mean_motion)
# qqline(differences$brouwer_mean_motion)
# 
# # For CryoSat-2, brouwer mean motion = clearly not normal
# 
# # What about the normalised differences
# differences <- differences %>% 
#   mutate(
#     norm_res = (brouwer_mean_motion - mean(brouwer_mean_motion))/sd(brouwer_mean_motion)
#   )
# 
# hist(differences$norm_res)
# 
# qqnorm(differences$norm_res)
# qqline(differences$norm_res)
# 
# # Still CLEARLY NOT NORMAL



# ggplot(data = differences, aes(x = brouwer_mean_motion)) + 
#   geom_histogram()


# # Set the number of standard deviations above which we classify as anomalous
# n_sd <- 2
# 
# # Calculate threshold based on standard deviation
# threshold <- sd(differences[[orbital_element]]) * n_sd
# 
# # Set anomaly point predictions where the difference is above the threshold
# differences <- differences %>% 
#   mutate(
#     anomalous = 1*(abs(brouwer_mean_motion) > threshold) # NEED TO FIX THIS IS A REALLY BIG MAJOR PROBLEM ISSUE LOOK HERE !!!!!
#   )


arima_anomalies <- function(data, orbital_element, threshold){
  # `data` must be a data frame with the following variables:
  # * date = the date of the observations
  # * the 6 orbital elements, with names in snake case
  # * manoeuvre = an indicator of whether a true manoeuvre occurred at the time
  
  # Require: tidyverse
  
  # res_data <- get_residuals(data, scaled = TRUE)
  # satID <- res_data$satellite_ID[1]
  
  # Use auto.arima to automatically fit an ARIMA model
  model <- auto.arima(y = data[[orbital_element]])

  # Add the residuals to the data frame, and normalise.
  result <- tibble(date = data$date,
                   observed = data[[orbital_element]],
                   manoeuvre = data$manoeuvre,
                   mod_residuals = model$residuals,
                   # Normalise the residuals and assume a normal distribution
                   score = anomaly_score(mod_residuals))
  
  # Detect anomalies based on the threshold
  anomalies <- result %>% 
    filter(score >= threshold)
  #filter(mod_residuals < -threshold | mod_residuals > threshold)
  
  result <- result %>% 
    mutate(predict_anomaly = ifelse(date %in% anomalies$date, 1, 0))
  
  return(result)
}

isolation_forest_anomalies_univariate <- function(data, orbital_element, threshold){
  # `data` must be a data frame with the following variables:
  # * date = the date of the observations
  # * the 6 orbital elements, with names in snake case
  # * manoeuvre = an indicator of whether a true manoeuvre occurred at the time
  
  # Require: tidyverse, isotree
  
  # Set up a data frame of only the date and the desired orbital element,
  # to input into Isolation Forest
  IF_input <- data %>% 
    select(date, all_of({{orbital_element}}))
  
  # Run isolation forest (from isotree package)
  model <- isolation.forest(IF_input)
  
  # Get assigned probabilities
  model_scores <- predict(model, IF_input)
  
  # Add the residuals to the data frame, and normalise.
  result <- tibble(date = data$date,
                   observed = data[[orbital_element]],
                   manoeuvre = data$manoeuvre,
                   model_score = model_scores,
                   # Normalise the model scores and assume a normal distribution
                   score = anomaly_score(model_score)
                   )
  
  # Detect anomalies based on the threshold
  anomalies <- result %>%
    filter(score >= threshold)
  
  result <- result %>%
    mutate(predict_anomaly = ifelse(date %in% anomalies$date, 1, 0))
  
  return(result)
}

isolation_forest_anomalies_multivariate <- function(data, orbital_elements = c("eccentricity", "argument_of_perigee", "inclination", "mean_anomaly", "brouwer_mean_motion", "right_ascension"), threshold){
  # `data` must be a data frame with the following variables:
  # * date = the date of the observations
  # * the 6 orbital elements, with names in snake case
  # * manoeuvre = an indicator of whether a true manoeuvre occurred at the time
  
  # `variables` must be a character column vector of the orbital elements to use
  
  # Require: tidyverse, isotree
  
  # Set up a data frame of only the date and the orbital elements,
  # to input into Isolation Forest
  IF_input <- data %>% 
    select(date,
           all_of({{orbital_elements}}))
  
  # Run isolation forest (from isotree package)
  model <- isolation.forest(IF_input)
  
  # Get assigned probabilities
  model_scores <- predict(model, IF_input)
  
  # Add the residuals to the data frame, and normalise.
  result <- tibble(date = data$date,
                   # observed = data[[orbital_element]],
                   manoeuvre = data$manoeuvre,
                   model_score = model_scores,
                   # Normalise the model scores and assume a normal distribution
                   score = anomaly_score(model_score)
  )
  
  # Detect anomalies based on the threshold
  anomalies <- result %>%
    filter(score >= threshold)
  
  result <- result %>%
    mutate(predict_anomaly = ifelse(date %in% anomalies$date, 1, 0))
  
  return(result)
}

