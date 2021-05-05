#' Plot time series forecast
#'
#' @param .optim_output optimization output.
#' @param interactive logical: Whether or not to return interative plotly graph
#' @param multiple_keys Logical. The data has or not multiple keys to plot as grid.
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @return graph
#' @export
#'
#' @examples
#' \dontrun{
#' plot_ts()
#' }
plot_ts <- function(.optim_output, interactive = FALSE, multiple_keys = FALSE){
  
  # Prescription
  
  prescription <- attributes(.optim_output)[["prescription"]]
  
  # Colors
  
  cols <- c("#000000", "#1B9E77", "#D95F02", "#7570B3", "#E7298A"
            , "#66A61E", "#E6AB02", "#A6761D", "#666666"
            , "#fa26a0", "#fa1616", "#007892")[1:length(unique(.optim_output$model))]
  
  names(cols) <- unique(.optim_output$model)
  
  graph_theme_int <- function(){
    theme_bw() %+replace%
      theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold", vjust = 2),
            plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold", vjust = 1.5),
            axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
            axis.title = element_text(size = 13, hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
  }
  
  if(attributes(.optim_output)[["output_type"]] == "optim_output_pi"){ # Option with pred intervals
    
    graph_tmp <- .optim_output %>% 
      group_by(date_var) %>% 
      mutate(lower_threshold = median(lower_threshold)
             , upper_threshold = median(upper_threshold)) %>% 
      ungroup() %>% 
      ggplot() +
      geom_line(aes(date_var, y_var, col = model), size = 1.01)+
      scale_colour_manual(values = cols)+
      geom_ribbon(aes(date_var
                      , ymin = lower_threshold
                      , ymax = upper_threshold), alpha = .2)+
      labs(x="Time",y = "Quantity (95% prediction interval)", title = "Generated Forecast"
           , subtitle = paste0("Selected Key:"," ", unique(.optim_output$key))
           , col = "Model")+
      geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed") +
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      scale_x_date(expand = c(0,0),date_breaks = "2 month", minor_breaks = NULL) +
      graph_theme_int()
    
  } else if(attributes(.optim_output)[["output_type"]] == "optim_output") { # Optim output
    
    graph_tmp <- .optim_output %>% 
      ggplot() +
      geom_line(aes(date_var, y_var, col = model), size = 1.01)+
      scale_colour_manual(values = cols)+
      labs(x="Time",y = "Quantity", title = "Generated Forecast"
           , subtitle = paste0("Selected Key:"," ", unique(.optim_output$key))
           , col = "Model")+
      geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed") +
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      scale_x_date(expand = c(0,0),date_breaks = "3 month", minor_breaks = NULL) +
      graph_theme_int()
    
  } else { # Error of input
    stop("Error, the input data is not class optim_output")
  }
  
  if(multiple_keys == TRUE){ # Multiple keys
    
    graph_tmp <- graph_tmp +
      facet_wrap( ~ key, scales = "free")
    
  }
  
  if(interactive == TRUE){ # Interactive
    
    ggplotly(graph_tmp)
    
  } else {
    
    graph_tmp
    
  }
  
}

#---
