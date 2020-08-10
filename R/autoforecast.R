
#' Autoforecast function
#'
#' @param data A data frame with a unique key, y (quantity), date and regressors
#' @param frequency By default: 12. Other frequencies will be allowed in next versions
#' @param models By default: "All". List of models to apply.
#' @param algo_study By default: TRUE. The function will automatically select the best models from the list of models of input and forecast. If it’s in FALSE, the function will just forecast with the selected models.
#' @param test_size By default: 6. It’s for the Algo study configuration
#' @param lag By default: 4. It’s for the Algo study configuration
#' @param output_models By default: 1. How many models for forecasting. You can select which models you want to output: Ex: (1,2,3) = Three best models.
#' @param parameters By default: All options in NULL. You can select different parameters for some algorithms
#' @param clean_series By default: TRUE. This will perform Kalman’s method where there is an attached regressor. (More methods in progress). This option will also remove 0’s before the time series.
#' @param allow_seas By default: TRUE. This will allow seasonal models to be explored and forecasted.
#' @param plot_output By default: TRUE. This will save the plots in memory.
#' @param h By default: 36. Forecasting horizon

# autoforecast

autoforecast <- function(data, frequency = 12, models, algo_study = TRUE, test_size = 6,
                         lag = 4, output_models = c(1:3), clean_series = TRUE, 
                         parameters = NULL, allow_seas = TRUE, plot_output = TRUE, h = 36){
  
  # Alerts ---------------------
  
  if(!frequency %in% c(12,52,365)){
    stop("Frequency not availabe")
  }
  
  # Fix output models ---------------------
  
  if(length(models)<length(output_models)){
    output_models <- c(1:length(models))
  }
  
  # Run all models option ---------------------

  if(models[1] == "All"){
    list_models <- c("naive","snaive","croston","ets","theta",
                     "arima","tbats","nn","prophet")
  }else{
    list_models <- models
  }

  # Frequency options for prophet and ime breaks for plotting ---------------------
  
  if(frequency == 12){
    time_breaks <- "2 month"
  }
  if(frequency == 52){
    time_breaks <- "2 weeks"
  }
  if(frequency == 365){
    time_breaks <- "2 days"
  }

  # If there are not keys, switch off cleansing and set a dummy key

  if(is.null(data$key)){ # Single time series case
    data$key <- "Single_Time_Series"
    clean_series <- FALSE # Switch off cleansing
  }
  
  # Unique keys ---------------------

  unique_keys <- unique(data$key) # Default data frame input
  nprod <- length(unique(data$key)) # Number of units to forecast

  # Main table for results storing ---------------------

  all_configs <- all_algo_study_outputs <- all_fcst <- matrix(nrow = 0, ncol = 0)
  all_plots <- list()

  # Sku loop ---------------------

  for(j in 1:nprod){ # Loop for sku

    sku_iter <- unique_keys[j] # Selected sku
    print(paste0("Working on"," ",sku_iter,";"," ",round(1-(j/nprod),2)*100,"%"," ","items pending")) # Print update

    if(class(data)[1] == "list"){ # Filter method: If it's a list, there's just one sku
      data_iter <- data
    }else{
      data_iter <- data %>% filter(key == sku_iter)
    }

    # for each key ---------------------

    if(clean_series == FALSE){

      # No cleansing + Building
      
      data0 <- data_iter %>% build_ts(frequency = frequency) # Build time series objects
      attr(data0,"frequency") <- frequency # Key as a data attribute
      attr(data0,"key") <- sku_iter # Key as a data attribute

      # algo_study without cleansing ---------------------

      if(algo_study == TRUE){

      if(nrow(data0[[2]])>12){ # If enough data to run algo_study

        algo_study_run <- data0 %>% algo_study(model = list_models,
                                               seas = allow_seas, parameters = parameters,
                                               test_size = test_size, lag = lag)

        config <- algo_study_run[[1]] # Configs
        algo_study_output <- algo_study_run[[2]] # Get Mape Results
        best_models <- as.data.frame(algo_study_output[output_models,1])
        best_models <- as.character(best_models[,1])

      }else{ # Set config info to NULL and forecast with croston

        config <- data.frame(frequency = "Not enough data", allow_seas = "Not enough data",
                             buckets = nrow(data0[[2]]), test_size = test_size, lag = lag)
        algo_study_output <- data.frame(model = "No models", cv_mape = "Not calculated")
        best_models <- as.character("croston")

      }

      }else{ # Algo Study Deactivated

        config <- data.frame(frequency = "Algo Study Deactivated", allow_seas = allow_seas,
                             buckets = nrow(data0[[2]]), test_size = test_size, lag = lag)
        algo_study_output <- data.frame(model = "No models", cv_mape = "Not calculated")
        best_models <- list_models

      }

      # gen_fcst without cleansing ---------------------

      fcst <- data0 %>% gen_fcst(models = best_models,
                                 seas = allow_seas, parameters = parameters,
                                 h = h)

      # Plot ---------------------
      
      plot <- plot_fcst(data = data0, forecast = fcst, key = sku_iter, breaks = time_breaks)

    }

    if(clean_series == TRUE){

      # Cleansing & Bulding ---------------------

      data0 <- data_iter %>% cleansing() %>% build_ts(frequency = frequency) # Cleansing + Remove 0's before active values
      attr(data0,"frequency") <- frequency # Key as a data attribute
      attr(data0,"key") <- sku_iter # Key as a data attribute

      # algo_study without cleansing ---------------------

      if(algo_study == TRUE){

      if(nrow(data0[[2]])>12){ # If enough data to run algo_study

        algo_study_run <- data0 %>% algo_study(model = list_models,
                                               seas = allow_seas, parameters = parameters,
                                               test_size = test_size, lag = lag)

        config <- algo_study_run[[1]] # Configs
        algo_study_output <- algo_study_run[[2]] # Get Mape Results
        best_models <- as.data.frame(algo_study_output[output_models,1])
        best_models <- as.character(best_models[,1])

      }else{ # Set config info to NULL and forecast with croston

        config <- data.frame(frequency = findfrequency(data0[[1]]), allow_seas = allow_seas,
                             buckets = nrow(data0[[2]]), test_size = test_size, lag = lag)
        algo_study_output <- data.frame(model = "No models", cv_mape = "Not calculated")
        best_models <- as.character("croston")

      }

      }else{ # Algo Study Deactivated

        config <- data.frame(frequency = "Algo Study Deactivated", allow_seas = allow_seas,
                             buckets = nrow(data0[[2]]), test_size = test_size, lag = lag)
        algo_study_output <- data.frame(model = "No models", cv_mape = "Not calculated")
        best_models <- list_models

      }

      # gen_fcst with cleansing ---------------------

      fcst <- data0 %>% gen_fcst(models = best_models,
                                 seas = allow_seas, parameters = parameters,
                                 h = h)
      
      # Plot ---------------------

      plot <- plot_fcst(data = data0, forecast = fcst, key = sku_iter, breaks = time_breaks)

    } # End cleansing loop

    # Storage ---------------------

    # Add key to table

    config <- cbind(rep(sku_iter,nrow(config)),config)
    colnames(config)[1] <- "key"
    algo_study_output <- cbind(rep(sku_iter,nrow(algo_study_output)),algo_study_output)
    colnames(algo_study_output)[1] <- "key"
    fcst <- cbind(rep(sku_iter,nrow(fcst)),fcst)
    colnames(fcst)[1] <- "key"

    # Add key rows to main table ---------------------

    all_configs <- rbind(all_configs, config)
    all_algo_study_outputs <- rbind(all_algo_study_outputs,algo_study_output)
    all_fcst <- rbind(all_fcst, fcst)
    all_plots[[j]] <- plot

  } # End sku loop

  if(plot_output == TRUE){

    return(list(configuration = all_configs,algo_study = all_algo_study_outputs,
                forecast = all_fcst, plots = all_plots)) # Output with plot

  }else{

    return(list(configuration = all_configs,algo_study = all_algo_study_outputs,
                forecast = all_fcst))

  }

}
