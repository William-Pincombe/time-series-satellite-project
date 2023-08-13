plot_satellite_data <- function(data){
  # Load packages to make the plot
  require(tidyverse, patchwork)
  
  # Initialise object to store plots
  plots <- list()
  
  # Plot each time series
  series_list <- c("eccentricity","argument_of_perigee","inclination",
                   "mean_anomaly","brouwer_mean_motion","right_ascension")
  for (i in 1:6){
    plots[[i]] <- ggplot(aes(x = date, y = .data[[series_list[i]]]), data = data) + 
      geom_line()
  }
  
  # Use patchwork to combine the plots
  return((plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]] + plots[[6]]))
}

