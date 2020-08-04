
# plot_fcst

plot_fcst <- function(data, forecast, key = NULL){

if(!is.null(key)){
  subtitle <- paste0("Key:"," ",key)
}else{
  subtitle <- paste0("Horizon:"," ",nrow(forecast)/length(unique(forecast$model)))
}

# Datasets

prev_data <- data[[2]] # Tibble
fcst_results <- forecast # Fcsted

plot <- ggplot() +
  geom_line(data = prev_data, aes(x= date, y = y), size = 1.0005) +  # Prev Sales
  geom_line(data = fcst_results, aes(x = time, y = predicted, color = model), size =  1.0005) +  # Data from models
  geom_vline(xintercept = as.numeric(ymd(min(fcst_results$time))), linetype="dashed",color = "red", size=1.0010) + # Black vline
  geom_vline(xintercept = as.numeric(ymd(max(prev_data$date))),color = "black", size=1.0010) + # Black vline
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) +
  labs(x="Time",y="Sales", title = "Forecast Results", subtitle = subtitle, color = "Model") +
  scale_x_date(expand = c(0,0), date_breaks = "2 month", date_labels = "%b-%y")

# Plotting

return(plot)

}

#---
