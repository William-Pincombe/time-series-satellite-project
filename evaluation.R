# FUNCTIONS FOR EVALUATING THE ANOMALY POINT PREDICTIONS

# All functions assume input data frames have the following columns:
# "score" containing a predicted anomaly probability
# "predict_anomaly" containing 1 if we predict an anomaly, 0 otherwise
# "manoeuvre" containing 1 if there was indeed a 

# OPTION 1: consider predictions to apply to a previous period at a certain distance

evaluate_leading <- function(data, n){
  # n is the number of periods we believe it takes for a manoeuvre to show up
  #   as an anomaly
  
  # Turn score and predict_anomaly into leading terms
  data <- data %>% 
    mutate(
      score = lead(score, n),
      predict_anomaly = lead(predict_anomaly, n)
    )
  
  # Remove the last n observations (since no longer useful)
  data <- data[1:(nrow(data) - n),]
  
  return(data)
}


# OPTION 2: moving window

evaluate_moving_window <- function(data, d){
  # d is the number of periods after a manoeuvre within which we expect the 
  #   prediction to appear (sort of like a 'maximum distance')
  
  # Define new vector for whether the moving window predicts an anomaly
  predict_anomaly_MW <- numeric(nrow(data))
  
  # For each period, check if there is a prediction in the period or in the
  # subsequent d periods
  for(i in 1:(nrow(data) - d)){
    for(j in 0:d){
      # If there is a prediction, then let the moving window detect an anomaly
      if(data$predict_anomaly[i+j] == 1){
        predict_anomaly_MW[i] <- 1
      }
    }
  }
  
  # Save over the predictions in the data
  data$predict_anomaly <- predict_anomaly_MW
  
  # Also need to get a probability for each period based on
  # those in the two periods in the moving window  
  
  # Initialise vector for new probabililties
  new_score <- numeric(nrow(data))
  
  # Set the probability as the maximum of the probabilities over the moving window
  for(i in 1:(nrow(data) - d)){
    new_score[i] <- max(data$score[i:(i+d)])
  }
  
  # Save over the old probability
  data$score <- new_score
  
  # Remove the last d periods
  data <- data[1:(nrow(data) - d),]
  
  return(data)
}





# Plot Precision-Recall curve based on the predicted anomaly probability (normality assumption?!?)
plot_pr_curve <- function(data){
  # Assumes the object data has columns:
  # "score" containing a predicted probability of a manoeuvre
  # "manoeuvre" containing 1 if there was a manoeuvre, 0 otherwise
  
  # Requires: tidyverse, yardstick
  
  # Plot the Precision-Recall curve,
  # being careful about the order of classes ("1" is the SEOCND class)
  p <- data %>% 
    mutate(manoeuvre = factor(manoeuvre, levels = c(0,1))) %>% 
    select(manoeuvre, score) %>% 
    pr_curve(manoeuvre, score, event_level = "second") %>% 
    ggplot(aes(x = recall, y = precision)) +
    geom_path() +
    #coord_equal() +
    labs(
      x= "Recall",
      y = "Precision"
      ) +
    theme_bw()
  
  # print(p)
  return(p)
}




# Plot ROC curve based on the predicted anomaly probability (normality assumption?!?)
plot_roc_curve <- function(data){
  # Assumes the object data has columns:
  # "score" containing a predicted probability of a manoeuvre
  # "manoeuvre" containing 1 if there was a manoeuvre, 0 otherwise
  
  # Requires: tidyverse, yardstick
  
  # Plot the ROC curve
  # being careful about the order of classes ("1" is the SEOCND class)
  p <- data %>% 
    mutate(manoeuvre = factor(manoeuvre, levels = c(0,1))) %>% 
    select(manoeuvre, score) %>% 
    roc_curve(manoeuvre, score, event_level = "second") %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 3) +
    #coord_equal() +
    labs(x= "1 - Specificity",
         y = "Sensitivity") +
    theme_bw()
  
  # print(p)
  return(p)
}


# Function to calculate the accuracy, precision, recall, F1 and specificity
# for after the moving window evaluation has been 
get_metrics <- function(data){
  # data must be a data frame including
  # manoeuvre = 1 if there is a manoeuvre, 0 otherwise
  # predict_anomaly = 1 if we predict, 0 otherwise
  
  # True positives
  TP <- nrow(filter(data, manoeuvre == 1, predict_anomaly == 1))
  
  # True negatives
  TN <- nrow(filter(data, manoeuvre == 0, predict_anomaly == 0))
  
  # False positives
  FP <- nrow(filter(data, manoeuvre == 0, predict_anomaly == 1))
  
  # False negatives
  FN <- nrow(filter(data, manoeuvre == 1, predict_anomaly == 0))
  
  # Accuracy
  accuracy <- (TP + TN)/(TP + TN + FP + FN)
  
  # Precision
  precision <- TP/(TP + FP)
  
  # Recall (aka sensitivity)
  recall <- TP/(TP + FN)
  
  # F1
  F1 <- (2*TP)/(2*TP + FP + FN)
    
  # Specificity
  specificity <- TN/(TN + FP)
      
  # Print metrics
  print(paste("Accuracy:", round(accuracy,4)))
  print(paste("Precision:", round(precision,4)))
  print(paste("Recall:", round(recall,4)))
  print(paste("F1:", round(F1,4)))
  print(paste("Specificity:", round(specificity,4)))
  
  return(c(accuracy, precision, recall, F1, specificity))
}

# Function to plot predicted anomalies
plot_predictions <- function(results, date_range = NULL){
  # `results` should contain the columns:
  # observed = observed values of the orbital element
  # manoeuvre == 1 if there is a manoeuvre
  # predict_anomaly == 1 if the applied method (post-evaluation) predicts an anomaly
  
  # Subset data, if a specific date range is provided
  if(!(is_null(date_range))){
    data <- data %>% 
      filter(date > date_range[1]) %>% 
      filter(date < date_range[2])
  }

  # Get just the manouevres
  manoeuvres_df <- filter(results, manoeuvre == 1)
    
  # Get just the predictions
  predictions_df <- filter(results, predict_anomaly == 1)
    
  return(results %>% 
    ggplot(aes(x = date, y = observed)) + 
      geom_line() +
      geom_point(data = manoeuvres_df, aes(color = "Manoeuvres"), size = 1) +
      # geom_vline(data = predictions_df, 
      #            aes(xintercept = date), 
      #            color = 'blue',
      #            linetype = 'dashed') +
      geom_point(data = predictions_df,
                 aes(x = date, y = observed, color = "Predictions"),
                 shape = 4,
                 alpha = 0.5) + 
      labs(x = "Time") + 
      scale_colour_manual(name = "Legend",
                          breaks = c("Manoeuvres", "Predictions"),
                          values = c("Manoeuvres" = "red", "Predictions" = "blue")) + 
      theme_bw()
  )
}


# # Predict manoeuvre if there is an anomaly detected in any of the 
# d <- 4
# str = string(d)
# for (i in seq(1,d)){
#   str[i] = paste("lead(anomalous, n = ",i,") == TRUE")
# }
# data %>% 
#   mutate(
#     predict_manoeuvre = if_else(anomalous == TRUE | lead(anomalous) == TRUE | lead(anomalous, n = 2) == TRUE,
#                                 true = 1, false = 0)
#   )
# 
