
# plot_ts

plot_ts <- function(data, key = NULL, breaks = "3 month", labels = "%Y-%m-%d"){
  
  if(is.null(key)){
    key <- "Selected key"
  }
  subtitle <- paste0("Selected Key:"," ",key)
  
  # Datasets
  
  prev_data <- data # Tibble
  plot <- ggplot() +
    geom_line(data = prev_data, aes(x = date, y = y), size = 1.0005) +  # Prev Sales
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold"),
          axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
          axis.title = element_text(size = 13, hjust = 0.5, face = "bold"),
          legend.position = "right",
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13)) +
    labs(x="Time",y="Sales", title = "Time Series", subtitle = subtitle) +
    scale_x_date(expand = c(0,0), date_breaks = breaks, date_labels = labels)
  
  # Plotting
  
  return(plot)
  
}

#---