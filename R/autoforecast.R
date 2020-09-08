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
    get_dyn_theta(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "prophet"){
    get_dyn_theta(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "tslm"){
    get_tslm(.data = .data, y_var = y_var, parameter = parameter)
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
#' @import fastDummies
#' @import foreach
#' @importFrom purrr map
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
autoforecast <- function(.data, parameter, test_size = 6, lag = 3, horizon = 36, model, optim_profile
                         , meta_data = FALSE, tune_parallel = FALSE, number_best_models = 3
                         , ensemble = FALSE,...){
  
  # utils::globalVariables(c("y_var_fcst", ".", "key", "y_var", "type", "date_var"))
  # y_var_fcst <- . <- key <- y_var <- type <- date_var <- NULL
  
  # default models
  if(model == "default"){
    model <- model_list <- c("glm", "glmnet", "arima", "ets", "dynamic_theta", "seasonal_naive", "croston")
  }
  
  # internal lag
  lag <- lag+1
  
  # consinstency checks
  if(length(model) > number_best_models){
    number_best_models <- length(model)
  }
  
  # get default parameters
  
  if(is.null(parameter)){
    # Glm and glmnet
    grid_glmnet <- expand_grid(time_weight = seq(from = 0.9, to = 1, by = 0.02)
                               , trend_discount = seq(from = 0.95, to = 1, by = 0.01)
                               , alpha = seq(from = 0, to = 1, by = 0.10))
    grid_glm <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.02)
                            , trend_discount = seq(from = 0.8, to = 1, by = 0.02))
    # parameter list
    parameter <- list(glmnet = list(time_weight = .94, trend_discount = .70, alpha = 0, lambda = .1
                                    , grid_glmnet = grid_glmnet
                                    , job = list(optim_lambda = TRUE, x_excluded = NULL
                                                 , random_search_size = 0.05
                                                 , n_best_model = 1))
                      , croston = list(alpha = 0.1)
                      , glm = list(time_weight = .99, trend_discount = 0.70
                                   , grid_glm = grid_glm
                                   , job = list(x_excluded = NULL
                                                , random_search_size = 0.1
                                                , n_best_model = 1))
                      , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                      , ets = list(ets = "ZZZ"))
  }
  
  # feature engineering & data cleansing 
  cat("\nProcedures applied: \n- Feature engineering \n- Cleansing\n");
  .data_tmp <- .data  %>% 
    feature_engineering_ts() %>% 
    clean_ts()
  
  # check data quality
  if(nrow(.data_tmp) < 12 | cumsum(.data_tmp$y_var) == 0){
    model <- "croston"
  }
  
  # get_forecast_int
  get_forecast_int <- function(.data, model, horizon, parameter){
    .data %>% 
      fit_ts(model = model, parameter = parameter) %>% 
      get_forecast(horizon = horizon)
  }
  
  # optim_join
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
  
  # fast profile
  if(optim_profile == "fast"){
    
    cat(paste0("\nFast optimization for: ", length(model), " Models + Unweighted Ensemble Forecast"))
    forecast_tmp <- map(model, ~get_forecast_int(.data = .data_tmp
                                               , model = .x, horizon = horizon
                                               , parameter = parameter)) %>% 
      bind_rows() %>% 
      mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history"))
    
    ensemble_tmp <- forecast_tmp %>% 
      dplyr::filter(type != "history") %>% 
      dplyr::group_by(date_var) %>% 
      dplyr::summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")
    
    forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp) %>% 
      fill(key, .direction = "down")
    
    # ensemble mode: Just output ensemble + conf interval
    if(ensemble == TRUE){
      # Calculate Pred Intervals
      ts_object <- ts(.data$y_var, start = c(1,1), freq = 12)
      ts_decomp <- stl(ts_object, s.window = "periodic")
      coef <- sd(ts_decomp$time.series[,3])
      ensemble_down <- ensemble_tmp %>% 
        dplyr::mutate(y_var = y_var - 1.5*coef,
                      type = "down_limit")
      ensemble_up <- ensemble_tmp %>% 
        dplyr::mutate(y_var = y_var + 1.5*coef,
                      type = "upper_limit")
      forecast_tmp <- bind_rows(forecast_tmp, ensemble_down, ensemble_up, ensemble_tmp) %>%
        fill(key, .direction = "down")
      forecast_tmp <- forecast_tmp %>% 
        dplyr::filter(model %in% c("history","ensemble"))
      attr(forecast_tmp, "output_type") <- "ensemble"
      
      # Main output  
      
    }else{ 
      forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp) %>%
        fill(key, .direction = "down")
      attr(forecast_tmp, "output_type") <- "optim_output"
    }
    
  # light profile  
  } else if(optim_profile == "light"){
    
    # slow models
    model <- setdiff(model, c("tbats","neural_network"))
    
    best_model_int <- optim_ts(.data_tmp, test_size = test_size, lag = lag
                               , parameter = parameter, model = model
                               , tune_parallel = tune_parallel)
    
    print(knitr::kable(best_model_int, "simple", 2))
    
    # get n best models
    model <- best_model_int %>% 
      dplyr::filter(ranking <= number_best_models) %>% 
      .[["model"]]
    
    forecast_tmp <- map(model
                        , ~optim_join(.data_tmp, model = .x, parameter = parameter
                                                , horizon = horizon, best_model = best_model_int)) %>% 
      bind_rows() %>% 
      mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history")) 
    
    ensemble_tmp <- forecast_tmp %>% 
      dplyr::filter(type != "history") %>% # top n models cv
      dplyr::group_by(date_var) %>% 
      dplyr::summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")
    
    # ensemble mode: Just output ensemble + conf interval
    if(ensemble == TRUE){
      # Calculate Pred Intervals
      ts_object <- ts(.data$y_var, start = c(1,1), freq = 12)
      ts_decomp <- stl(ts_object, s.window = "periodic")
      coef <- sd(ts_decomp$time.series[,3])
      ensemble_down <- ensemble_tmp %>% 
        dplyr::mutate(y_var = y_var - 1.5*coef,
                      type = "down_limit")
      ensemble_up <- ensemble_tmp %>% 
        dplyr::mutate(y_var = y_var + 1.5*coef,
                      type = "upper_limit")
      forecast_tmp <- bind_rows(forecast_tmp, ensemble_down, ensemble_up, ensemble_tmp) %>%
        fill(key, .direction = "down")
      forecast_tmp <- forecast_tmp %>% 
        dplyr::filter(model %in% c("history","ensemble"))
      attr(forecast_tmp, "output_type") <- "ensemble"
      
    # main output  
    }else{ 
      forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp) %>%
        fill(key, .direction = "down")
      attr(forecast_tmp, "output_type") <- "optim_output"
    }
    
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
#'     stop("Error, the input data is not class optim_output")
plot_ts <- function(.optim_output, interactive = FALSE, multiple_keys = FALSE){
  prescription <- attributes(.optim_output)[["prescription"]]
  if(attributes(.optim_output)[["output_type"]] == "ensemble"){ # Ensemble option
    # key
    subtitle <- paste0("Selected Key:"," ",unique(.optim_output$key))
    # graph
    graph_tmp <- .optim_output %>% 
      ggplot(aes(date_var, y_var, col = type), size = 1.0005) +
      geom_line(size = 1.0005) +
      labs(x = "", y = "y_var", col = "Model") +
      geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed") +
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      scale_x_date(expand = c(0,0),date_breaks = "2 month", minor_breaks = NULL) +
      theme_bw() +
      theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold"),
            axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
            axis.title = element_text(size = 13, hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13)) +
      labs(x="Time",y="Sales", title = "Ensemble forecast", subtitle = subtitle)
  } else if(attributes(.optim_output)[["output_type"]] == "optim_output") { # Optim output
    # key
    subtitle <- paste0("Selected Key:"," ",unique(.optim_output$key))
    # graph
    graph_tmp <- .optim_output %>% 
      ggplot(aes(date_var, y_var, col = model), size = 1.0005)+
      geom_line(size = 1.0005)+
      labs(x = "", y = "y_var", col = "Model")+
      geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed")+
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      scale_x_date(expand = c(0,0),date_breaks = "2 month", minor_breaks = NULL)+
      theme_bw() +
      theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold"),
            axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
            axis.title = element_text(size = 13, hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13)) +
      labs(x="Time",y="Sales", title = "Generated Forecast", subtitle = subtitle)
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
