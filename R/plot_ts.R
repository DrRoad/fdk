#' Plot time series forecast
#'
#' @param .fdk optimization output.
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
plot_ts <- function(.fdk, interactive = FALSE, multiple_keys = FALSE, .prescribed_data = NULL, top_mape = NULL){
  
  fdk_class <- attributes(.fdk)[["fdk_class"]]
  key_int <- attributes(.fdk)[[".log"]][["key"]]
  if(is.numeric(top_mape) == FALSE){
    top_mape <- nrow(.fdk)
  }
  
  if(is.null(.prescribed_data) == F){
    tmp_data <- get(.prescribed_data) %>% 
      filter(key == key_int) %>% 
      pull(data) %>% 
      .[[1]] %>% 
      validate_ts() %>% 
      feature_engineering_ts() %>% 
      clean_ts() %>% 
      dplyr::select(date_var, y_var)
  }
  
  if(fdk_class %in% c("optim_ts", "pipeline_ts")){
    
    tmp <- .fdk %>%
      mutate(spa_deviation = spa - 1
             , rank =  rank(rank_agg)
             , rank = ifelse(rank <= 3
                             , rank, NA_real_))
    means <- tmp %>% 
      group_by(model) %>% 
      summarise(across(.cols = c("mape", "spa")
                       , .fns = ~median(.x))) %>% 
      mutate(rank = paste0("_", model))
    
    tmp %>% 
      mutate(rank = as.character(rank)) %>% 
      bind_rows(means) %>% 
      ggplot()+
      geom_density_2d_filled(aes(mape, spa), na.rm = T
                             , show.legend = F, alpha = .87)+
      geom_hline(yintercept = 1, linetype = "dashed"
                 , col = "white")+
      geom_point(aes(mape, spa, shape = model)
                 , size = 2, col = "black")+
      geom_label_repel(aes(mape, spa, label = rank)
                       , alpha = .76, na.rm = T)+
      theme_minimal()+
      labs(y = "SPA", x = "MAPE"
           , shape = "Model")+
      scale_x_log10()+
      scale_y_log10()
  } else if(all(c("forecast_ts", "optim_ts") %in% fdk_class)){
    if(is.null(.prescribed_data)==F){
      tmp_data %>% 
        bind_rows(
          .fdk %>% 
            dplyr::select(index, forecast, mape) %>% 
            top_n(n = top_mape, wt = -mape) %>% 
            mutate(index = as.character(index)) %>% 
            unnest(forecast)
        ) %>% 
        mutate(quantity = case_when(
          is.na(forecast) ~ y_var
          , TRUE ~ forecast)
          , is_history = case_when(
            is.na(forecast) ~ TRUE
            , TRUE ~ FALSE
          )) %>% 
        ggplot(aes(date_var, quantity, col = index))+
        geom_line()
    } else {
      .fdk %>% 
        dplyr::select(index, forecast, mape) %>% 
        mutate(index = as.character(index)) %>% 
        unnest(forecast) %>% 
        ggplot(aes(date_var, forecast, col = index))+
        geom_line()
    }
  } else if(fdk_class == "forecast_ts"){
    
    if(is.null(.prescribed_data) == F){
      tmp_data %>% 
        bind_rows(.fdk) %>% 
        mutate(quantity = case_when(
          is.na(forecast) ~ y_var
          , TRUE ~ forecast)
          , is_history = case_when(
            is.na(forecast) ~ TRUE
            , TRUE ~ FALSE
          )) %>% 
        ggplot()+
        geom_line(aes(date_var, quantity, col = is_history))
    } else {
      .fdk %>% 
        ggplot()+
        geom_line(aes(date_var, forecast))
    }
  }
  
  # # Prescription
  # 
  # prescription <- attributes(.optim_output)[["prescription"]]
  # 
  # # Colors
  # 
  # cols <- c("#000000", "#1B9E77", "#D95F02", "#7570B3", "#E7298A"
  #           , "#66A61E", "#E6AB02", "#A6761D", "#666666"
  #           , "#fa26a0", "#fa1616", "#007892")[1:length(unique(.optim_output$model))]
  # 
  # names(cols) <- unique(.optim_output$model)
  # 
  # graph_theme_int <- function(){
  #   theme_bw() %+replace%
  #     theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold", vjust = 2),
  #           plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold", vjust = 1.5),
  #           axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
  #           axis.title = element_text(size = 13, hjust = 0.5, face = "bold"),
  #           legend.position = "right",
  #           legend.title = element_text(size = 15),
  #           legend.text = element_text(size = 13))
  # }
  # 
  # if(attributes(.optim_output)[["output_type"]] == "optim_output_pi"){ # Option with pred intervals
  #   
  #   graph_tmp <- .optim_output %>% 
  #     group_by(date_var) %>% 
  #     mutate(lower_threshold = median(lower_threshold)
  #            , upper_threshold = median(upper_threshold)) %>% 
  #     ungroup() %>% 
  #     ggplot() +
  #     geom_line(aes(date_var, y_var, col = model), size = 1.01)+
  #     scale_colour_manual(values = cols)+
  #     geom_ribbon(aes(date_var
  #                     , ymin = lower_threshold
  #                     , ymax = upper_threshold), alpha = .2)+
  #     labs(x="Time",y = "Quantity (95% prediction interval)", title = "Generated Forecast"
  #          , subtitle = paste0("Selected Key:"," ", unique(.optim_output$key))
  #          , col = "Model")+
  #     geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed") +
  #     scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
  #     scale_x_date(expand = c(0,0),date_breaks = "2 month", minor_breaks = NULL) +
  #     graph_theme_int()
  #   
  # } else if(attributes(.optim_output)[["output_type"]] == "optim_output") { # Optim output
  #   
  #   graph_tmp <- .optim_output %>% 
  #     ggplot() +
  #     geom_line(aes(date_var, y_var, col = model), size = 1.01)+
  #     scale_colour_manual(values = cols)+
  #     labs(x="Time",y = "Quantity", title = "Generated Forecast"
  #          , subtitle = paste0("Selected Key:"," ", unique(.optim_output$key))
  #          , col = "Model")+
  #     geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed") +
  #     scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
  #     scale_x_date(expand = c(0,0),date_breaks = "3 month", minor_breaks = NULL) +
  #     graph_theme_int()
  #   
  # } else { # Error of input
  #   stop("Error, the input data is not class optim_output")
  # }
  # 
  # if(multiple_keys == TRUE){ # Multiple keys
  #   
  #   graph_tmp <- graph_tmp +
  #     facet_wrap( ~ key, scales = "free")
  #   
  # }
  # 
  # if(interactive == TRUE){ # Interactive
  #   
  #   ggplotly(graph_tmp)
  #   
  # } else {
  #   
  #   graph_tmp
  #   
  # }
  
}

#---
