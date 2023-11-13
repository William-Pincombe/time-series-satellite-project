# plot_satellite_data <- function(data){
#   # Load packages to make the plot
#   require(tidyverse, patchwork)
#   
#   # Initialise object to store plots
#   plots <- list()
#   
#   # Plot each time series
#   series_list <- c("eccentricity","argument_of_perigee","inclination",
#                    "mean_anomaly","brouwer_mean_motion","right_ascension")
#   for (i in 1:6){
#     plots[[i]] <- ggplot(aes(x = date, y = .data[[series_list[i]]]), data = data) + 
#       geom_line()
#   }
#   
#   # Use patchwork to combine the plots
#   return((plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]] + plots[[6]]))
# }




plot_satellite_data <- function(data, date_range = NULL){
  # Require packages for displaying plots
  require(tidyverse, patchwork)
  
  # Subset data, if a specific date range is provided
  if(!(is_null(date_range))){
    data <- data %>% 
      filter(date > date_range[1]) %>% 
      filter(date < date_range[2])
  }
  
  manoeuvres_df <- data %>% filter(manoeuvre == 1)
  # satID <- data$satellite_ID[1]
  
  # plot names
  plot_names <- c("Eccentricity","Argument of Perigee", "Inclination",
                  "Mean Anomaly","Brouwer Mean Motion", "Right Ascension")
  
  p1 <- ggplot(aes(x=date, y = eccentricity), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[1],
         x = "Time",
         y = "Value") +
    theme_bw()
  
  p2 <- ggplot(aes(x=date, y = argument_of_perigee), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[2],
         x = "Time",
         y = "Value") +
    theme_bw()
  
  p3 <- ggplot(aes(x=date, y = inclination), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[3],
         x = "Time",
         y = "Value") +
    theme_bw()
  
  p4 <- ggplot(aes(x=date, y = mean_anomaly), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[4],
         x = "Time",
         y = "Value") +
    theme_bw()
  
  p5 <- ggplot(aes(x=date, y = brouwer_mean_motion), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[5],
         x = "Time",
         y = "Value") +
    theme_bw()
  
  p6 <- ggplot(aes(x=date, y = right_ascension), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[6],
         x = "Time",
         y = "Value") +
    theme_bw()
  
  
  #store individual plots as a list
  plot_list <- list(p1,p2,p3,p4,p5,p6)
  
  
  # Use patchwork to combine the plots
  p <- (p1+p2+p3) / (p4 + p5 + p6)
  # big_plot <- p + 
  #   plot_annotation(title = sprintf("Unpropogated Orbital Elements"),
  #                   subtitle = "Red Points Denote Manouvre Times")
  
  return(p)
}

plot_satellite_data_diffs <- function(data, date_range = NULL){
  # Require packages for displaying plots
  require(tidyverse, patchwork)
  
  # Subset data, if a specific date range is provided
  if(!(is_null(date_range))){
    data <- data %>% 
      filter(date > date_range[1]) %>% 
      filter(date < date_range[2])
  }
  
  manoeuvres_df <- data %>% filter(manoeuvre == 1)
  # satID <- data$satellite_ID[1]
  
  # plot names
  plot_names <- c("Eccentricity","Argument of Perigee", "Inclination",
                  "Mean Anomaly","Brouwer Mean Motion", "Right Ascension")
  
  p1 <- ggplot(aes(x=date, y = eccentricity), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[1],
         x = "Time",
         y = "Difference") +
    theme_bw()
  
  p2 <- ggplot(aes(x=date, y = argument_of_perigee), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[2],
         x = "Time",
         y = "Difference") +
    theme_bw()
  
  p3 <- ggplot(aes(x=date, y = inclination), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[3],
         x = "Time",
         y = "Difference") +
    theme_bw()
  
  p4 <- ggplot(aes(x=date, y = mean_anomaly), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[4],
         x = "Time",
         y = "Difference") +
    theme_bw()
  
  p5 <- ggplot(aes(x=date, y = brouwer_mean_motion), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[5],
         x = "Time",
         y = "Difference") +
    theme_bw()
  
  p6 <- ggplot(aes(x=date, y = right_ascension), data = data) +
    geom_line() +
    geom_point(data = manoeuvres_df, color = 'red', size = 1) +
    labs(title = plot_names[6],
         x = "Time",
         y = "Difference") +
    theme_bw()
  
  
  #store individual plots as a list
  plot_list <- list(p1,p2,p3,p4,p5,p6)
  
  
  # Use patchwork to combine the plots
  p <- (p1+p2+p3) / (p4 + p5 + p6)
  # big_plot <- p + 
  #   plot_annotation(title = sprintf("Unpropogated Orbital Elements"),
  #                   subtitle = "Red Points Denote Manouvre Times")
  
  return(p)
}