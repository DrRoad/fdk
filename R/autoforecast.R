#' Fitting models for time-series data
#' 
#' Wrapper of single function call to simplify the estimation
#'
#' @param .data data-frame or tibble
#' @param y_var Column name of the variable to be forecasted
#' @param date_var Column name of the time index
#' @param model String. Name of the model to be estimated.
#' @param parameter List. Optional set of parameters to estimate models.
#'
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' fit_ts()
#' }
fit_ts <- function(.data, y_var, date_var, model, parameter = NULL){
  if(model == "glmnet"){
    get_glmnet(.data = .data, y_var = y_var, date_var = date_var, parameter = parameter)
  } else if(model == "glm"){
    get_glm(.data = .data, y_var = y_var, date_var = date_var, parameter = parameter)
  } else if(model == "ets"){
    get_ets(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "arima"){
    get_arima(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "neural_network"){
    get_neural_network(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "seasonal_naive"){
    get_seasonal_naive(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "tbats"){
    get_tbats(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "croston"){
    get_croston(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "dynamic_theta"){
    get_theta() # add
  }
}


#' Autoforecast
#' 
#' This function unifies all modules to bring an automatic forecast given a set of specification.
#'
#' @param .data data-frame or tibble
#' @param parameter List. Optional set of parameters to estimate models.
#' @param test_size Numeric. Number of splits to define the best hyperparameters/model.
#' @param lag Numeric. Number of periods ahead to assess model accuracy.
#' @param horizon Numeric. Number of periods in the future to generate the forecast.
#' @param model String. Name of the model to be estimated.
#' @param optim_profile String. Defines how strict show the model look for the best parameter. Options are:
#' fast, light, medium, and, complete.
#' @param meta_data Logical. Whether to return the ranking of models in a list.
#' @param ... Other parameter from the sub-functions.
#' @param tune_parallel Logical. Perform parallelization across different model selection (**experimental**).
#' 
#' @import stats
#' @import fastDummies
#' @import foreach
#' @import furrr
#' @import purrr
#' @import glmnet
#' @import dplyr
#' @import stlplus
#' @import forecast
#' @import imputeTS
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' autoforecast()
#' }
autoforecast <- function(.data, parameter, test_size, lag, horizon, model, optim_profile
                         , meta_data = FALSE, tune_parallel = FALSE, ...){
  
  cat("\nProcedures applied: \n- Feature engineering \n- Cleansing\n");
  
  .data_tmp <- .data  %>% 
    feature_engineering_ts() %>% 
    clean_ts(...)
  
  # Check data length
  
  if(nrow(.data_tmp) < 12 ){
    model <- "croston"
  }
  
  get_forecast_int <- function(.data, model, horizon, parameter){
    .data %>% 
      fit_ts(model = model, parameter = parameter) %>% 
      get_forecast(horizon = horizon)
  }
  
  optim_join <- function(.data, model, parameter, horizon, best_model){
    if(model %in% c("glmnet", "glm")){ # missing arima
      get_forecast_int(.data, model = model
                       , parameter = update_parameter(parameter
                                                      , best_model$parameter[[which(best_model$model==model)]]
                                                      , model = model, optim = TRUE)
                       , horizon = horizon)
    } else {
      get_forecast_int(.data, model = model, parameter = parameter, horizon = horizon)
    }
  }
  
  if(optim_profile == "fast"){
    
    cat(paste0("\nFast optimization for: ", length(model), " models + unweighted ensemble forecast"))
    forecast_tmp <- map(setdiff(model, c("tbats","neural_network")), ~get_forecast_int(.data = .data_tmp
                                               , model = .x, horizon = horizon
                                               , parameter = parameter)) %>% 
      bind_rows() %>% 
      mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      select(key:y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history"))
    
    ensemble_tmp <- forecast_tmp %>% 
      dplyr::filter(type != "history", (model != "neural_network")) %>% 
      group_by(date_var) %>% 
      summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")
    
    forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp) %>% 
      fill(key, .direction = "down")
    
    attr(forecast_tmp, "output_type") <- "optim_output"
    
  } else if(optim_profile == "light"){
    
    best_model_int <- optim_ts(.data_tmp, test_size = test_size, lag = lag
                               , parameter = parameter, model = setdiff(model, c("tbats","neural_network"))
                               , tune_parallel = tune_parallel)
    
    print(knitr::kable(best_model_int, "simple", 2))
    
    forecast_tmp <- map(setdiff(model, c("tbats","neural_network"))
                        , ~optim_join(.data_tmp, model = .x, parameter = parameter
                                                , horizon = horizon, best_model = best_model_int)) %>% 
      bind_rows() %>% 
      mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      select(key:y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history")) 
    
    ensemble_tmp <- forecast_tmp %>% 
      dplyr::filter(type != "history", model %in% best_model_int$model[1:3]) %>% # top 3 models cv
      group_by(date_var) %>% 
      summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")
    
    forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp) %>% 
      fill(key, .direction = "down")
    
    attr(forecast_tmp, "output_type") <- "optim_output"
    
  }
  
  if(meta_data == TRUE){
    return(list(forecast_output = forecast_tmp, ranking = best_model_int))
  } else {
    return(forecast_tmp)
  }
}

#' Plot time series forecast
#'
#' @param .data data-frame of class optim_output
#' @param interactive Logical. Whether or not to return interative plotly graph
#' @param multiple_keys Logical. The data has or not multiple keys to plot as grid.
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @return graph
#' @export
#'
#' @examples
#' \dontrun{
#' plot_ts()
#' }
plot_ts <- function(.data, interactive = FALSE, multiple_keys = FALSE){
  prescription <- attributes(.data)[["prescription"]]
  if(attributes(.data)[["output_type"]] != "optim_output"){
    stop("Error, the input data is not class optim_output")
  } else {
    graph_tmp <- .data %>% 
      ggplot(aes(date_var, y_var, col = model))+
      geom_line()+
      labs(x = "", y = "y_var", col = "Model")+
      geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed")+
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      scale_x_date(date_breaks = "6 month", minor_breaks = NULL)+
      theme_minimal()+
      theme(axis.text = element_text(size = 12)
            , axis.text.x = element_text(angle = 90))
    if(multiple_keys == TRUE){
      graph_tmp <- graph_tmp +
        facet_wrap(~key, scales = "free")
    }
    
    if(interactive == TRUE){
      ggplotly(graph_tmp)
    } else {
      graph_tmp
    }
  }
}
