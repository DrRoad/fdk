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
    suppressWarnings({get_prophet(.data = .data, y_var = y_var, parameter = parameter)})
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
#' @param number_best_models Integer. Among the best models, how many to output (also controls the Ensemble forecast).
#' @param pred_interval Logical. Control if prediction intervals are printed.
#' 
#' @import fastDummies
#' @import foreach
#' @importFrom purrr map
#' @import glmnet
#' @import dplyr
#' @import stlplus
#' @import forecast
#' @import imputeTS
#' 
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' autoforecast()
#' }
autoforecast <- function(.data, parameter, test_size = 6, lag = 3, horizon = 36, model, optim_profile
                         , meta_data = FALSE, tune_parallel = FALSE, number_best_models = 3
                         , pred_interval = FALSE, metric = "mape", ...){
  
  # Default models
  if(model == "default"){
    model <- c("glmnet","glm","prophet","dynamic_theta","arima","ets")
  }
  
  # Internal lag calculation
  lag <- lag+1
  
  # Consinstency checks
  if(length(model) > number_best_models){
    number_best_models <- length(model)
  }
  
  # Get default parameters
  if(is.null(parameter)){
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
  
  # Feature engineering & data cleansing 
  cat("\nProcedures applied: \n- Feature engineering \n- Cleansing\n");
  
  .data_tmp <- .data  %>% 
    feature_engineering_ts() %>% 
    clean_ts()
  
  # Check data quality
  quantity_test_size <- sum(.data_tmp[["y_var"]][(nrow(.data_tmp)-test_size):(nrow(.data_tmp))])

  if(nrow(.data_tmp) < 12 | cumsum(.data_tmp$y_var) == 0 | quantity_test_size == 0){
    model <- "croston"
  }
  

  # Internal functions ------------------------------------------------------

  # Ensemble intervals
  get_pred_interval_int <- function(.data_tmp, .forecast_output, z_score = 1.95){
    .data_ts_tmp <- ts(.data_tmp$y_var, start = c(1,1), freq = 12)
    .resid_ts_tmp <- stl(.data_ts_tmp, s.window = "periodic")$time.series[,3]
  
    pred_int_tmp <- .forecast_output %>% 
      rowwise() %>% 
      mutate(lower_threshold = case_when(type == "forecast" ~ y_var - z_score * sd(.resid_ts_tmp))
             , upper_threshold = case_when(type == "forecast" ~ y_var + z_score * sd(.resid_ts_tmp))) %>% 
      ungroup()
    
    return(pred_int_tmp)
  }
  
  # Calculate ensemble
  get_ensemble_int <- function(.forecast_tmp){
    ensemble_tmp <- .forecast_tmp %>% 
      dplyr::filter(type != "history") %>% # top n models cv
      dplyr::group_by(date_var) %>% 
      dplyr::summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")
  }
  
  # Generalized function to get forecasts
  get_forecast_int <- function(.data, model, horizon, parameter){
    .data %>% 
      fit_ts(model = model, parameter = parameter) %>% 
      get_forecast(horizon = horizon)
  }
  
  # Replace hyper-parameters on demand
  optim_join_int <- function(.data, model, parameter, horizon, best_model){
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
  
  if(optim_profile == "fast"){ # Fast profile ----------------------------------
    
    model <- setdiff(model, c("tbats","neural_network"))
    
    cat(paste0("\nFast optimization for: ", length(model), " Models + Unweighted Ensemble Forecast"))
    
    ## Generates forecast given default (hyper)parameters.
    forecast_tmp <- map(model, ~get_forecast_int(.data = .data_tmp
                                               , model = .x, horizon = horizon
                                               , parameter = parameter)) %>% 
      bind_rows() %>% 
      dplyr::mutate(y_var_fcst = ifelse(y_var_fcst < 0, 0, y_var_fcst)) %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history"))
    
    ## Ensemble is a simple average of every model's forecast.
    ensemble_tmp <- get_ensemble_int(.forecast_tmp = forecast_tmp)
    
    forecast_output <- bind_rows(forecast_tmp, ensemble_tmp) %>%
      fill(key, .direction = "down")
    
  } else if(optim_profile == "light"){ # Light profile -------------------------
    
    # slow models
    model <- setdiff(model, c("tbats","neural_network"))
    
    best_model_int <- optim_ts(.data_tmp, test_size = test_size, lag = lag
                               , parameter = parameter, model = model
                               , tune_parallel = tune_parallel
                               , metric = metric)
    
    print(knitr::kable(best_model_int, "simple", 2))
    
    # get n best models
    model <- best_model_int %>% 
      dplyr::filter(ranking <= number_best_models) %>% 
      .[["model"]]
    
    forecast_tmp <- map(model
                        , ~optim_join_int(.data_tmp, model = .x, parameter = parameter
                                                , horizon = horizon, best_model = best_model_int)) %>% 
      bind_rows() %>% 
      mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history")) 
    
    ensemble_tmp <- get_ensemble_int(.forecast_tmp = forecast_tmp)
    
    forecast_output <- bind_rows(forecast_tmp, ensemble_tmp) %>%
      fill(key, .direction = "down")
    
  }
  
  if(pred_interval == TRUE){
    
    forecast_output <- get_pred_interval_int(.data_tmp = .data_tmp
                                             , .forecast_output = forecast_output)
    
    attr(forecast_output, "prescription") <- attributes(.data_tmp)[["prescription"]]
    attr(forecast_output, "output_type") <- "optim_output_pi"
    
  } else { # Main output 
    attr(forecast_output, "prescription") <- attributes(.data_tmp)[["prescription"]]
    attr(forecast_output, "output_type") <- "optim_output"
  }
  
  if(meta_data == TRUE & optim_profile == "light"){
    return(list(forecast_output = forecast_output, ranking = best_model_int))
  } else {
    return(forecast_output)
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
plot_ts <- function(.optim_output, interactive = FALSE, multiple_keys = FALSE){
  prescription <- attributes(.optim_output)[["prescription"]]
  
  cols <- c("#000000", "#1B9E77", "#D95F02", "#7570B3", "#E7298A"
    , "#66A61E", "#E6AB02", "#A6761D", "#666666"
    , "#fa26a0", "#fa1616", "#007892")[1:length(unique(.optim_output$model))]

  names(cols) <- unique(.optim_output$model)
  
  graph_theme_int <- function(){
    theme_bw() %+replace%
      theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5, face = "bold"),
            axis.text.x = element_text(size = 11, angle = 90, hjust = 1),
            axis.title = element_text(size = 13, hjust = 0.5, face = "bold"),
            legend.position = "right",
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))
  }
  
  if(attributes(.optim_output)[["output_type"]] == "optim_output_pi"){ # Ensemble option
    
    graph_tmp <- .optim_output %>% 
      group_by(date_var) %>% 
      mutate(lower_threshold = median(lower_threshold)
             , upper_threshold = median(upper_threshold)) %>% 
      ungroup() %>% 
      ggplot() +
      geom_line(aes(date_var, y_var, col = model))+
      scale_colour_manual(values = cols)+
      geom_ribbon(aes(date_var
                      , ymin = lower_threshold
                      , ymax = upper_threshold), alpha = .2)+
      labs(x="Time",y = "Quantity (95% prediction interval)", title = "Forecast"
           , subtitle = paste0("Selected Key:"," ", unique(.optim_output$key))
           , col = "Model")+
      geom_vline(xintercept = as.Date(prescription$max_date), linetype ="dashed") +
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      scale_x_date(expand = c(0,0),date_breaks = "3 month", minor_breaks = NULL) +
      graph_theme_int()
    
  } else if(attributes(.optim_output)[["output_type"]] == "optim_output") { # Optim output
    graph_tmp <- .optim_output %>% 
      ggplot() +
      geom_line(aes(date_var, y_var, col = model))+
      scale_colour_manual(values = cols)+
      labs(x="Time",y = "Quantity", title = "Forecast"
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
