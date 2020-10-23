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
#' @importFrom lubridate ymd
#' @importFrom stlplus stlplus
#' @importFrom purrr map
#' @import fastDummies
#' @import foreach
#' @import glmnet
#' @import dplyr
#' @import stats
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
                         , pred_interval = FALSE, metric = "mape", method = "winsorize"
                         , frequency = 12, ...){
  
  # Set seed
  
  set.seed(123)
  
  # Internal lag calculation
  
  lag <- lag+1
  
  # Get default parameters
  
  if(is.null(parameter)){
    
    # Parameters -------------------------------------------------------------
    
    grid_glmnet <- expand_grid(time_weight = seq(from = 0.90, to = 1, by = 0.01)
                               , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1)
                               , alpha = seq(from = 0, to = 1, by = 0.25))
    grid_glm <- expand_grid(time_weight = seq(from = 0.90, to = 1, by = 0.01)
                            , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1))
    
    # Parameter list -------------------------------------------------------------
    
    parameter <- list(glmnet = list(time_weight = .94, trend_discount = .70, alpha = 0, lambda = .1
                                    , grid_glmnet = grid_glmnet
                                    , job = list(optim_lambda = TRUE, x_excluded = NULL
                                                 , random_search_size = 0.25
                                                 , n_best_model = 1))
                      , croston = list(alpha = 0.1)
                      , glm = list(time_weight = .99, trend_discount = 0.70
                                   , grid_glm = grid_glm
                                   , job = list(x_excluded = NULL
                                                , random_search_size = 0.25
                                                , n_best_model = 1))
                      , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                      , ets = list(ets = "ZZZ"))
    
  }
  
  # Feature engineering & data cleansing 
  
  cat("\nProcedures applied: \n- Feature engineering \n- Cleansing\n")
  
  # Main validation
  
  .data_tmp <- .data %>% validate_ts()
  
  # Validation attributes
  
  .main_attributes <- attributes(.data_tmp)

  # Main check forecasting rules
  
  if(.main_attributes$prescription$size < 12 | .main_attributes$prescription$intermittency > 0.35){
    .data_tmp <- .data_tmp %>% 
      feature_engineering_ts()
    optim_profile <- "fast"
    model <- "croston"
  }else{ # Check if data in test or 0 consistency
    quantity_test_size <- sum(.data_tmp[["y_var"]][(.main_attributes$prescription$size-test_size):(.main_attributes$prescription$size)])
    if(sum(.data_tmp$y_var) == 0 | quantity_test_size == 0){ # If not enough data, do feature engineering & select a croston
      .data_tmp <- .data_tmp %>% 
        feature_engineering_ts()
      optim_profile <- "fast"
      model <- "croston"
    }else{ # Else, do feature engineering & cleansing
      .data_tmp <- .data_tmp  %>% 
        feature_engineering_ts() %>% 
        clean_ts(method = method)
    }
  }
  
  # Internal functions ------------------------------------------------------

  # Ensemble intervals
  
  get_pred_interval_int <- function(.data_tmp, .forecast_output, z_score = 1.95){
    .data_ts_tmp <- ts(.data_tmp$y_var, start = c(1,1), freq = 12)
    if(length(.data_ts_tmp)>24){
    .resid_ts_tmp <- stl(.data_ts_tmp, s.window = "periodic")$time.series[,3]
    }else{
    .resid_ts_tmp <- .data_ts_tmp
    }
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
  
  # Weighted ensemble
  
  get_ensemble_int_weighted <- function(.forecast_tmp, .weights){
    
    # auxiliar
    
    .weights <- .weights %>% select(model,cv_metric) 
    
    # join
    
    .forecast_tmp_ratios <- left_join(.forecast_tmp,.weights) 
    .forecast_tmp_ratios <- .forecast_tmp_ratios %>% 
      dplyr::filter(type == "forecast")
    
    # calculate new forecast
    
    .forecast_tmp_weighted <- .forecast_tmp_ratios %>% 
      dplyr::group_by(model) %>% 
      dplyr::mutate(ind_metric = 1/abs(cv_metric)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(all_metric = sum(unique(ind_metric),na.rm = TRUE),
             coeff = ind_metric/all_metric) 
    
    # output
    
    ensemble_tmp <- .forecast_tmp_weighted %>% 
      dplyr::group_by(date_var) %>% 
      dplyr::mutate(y_var = y_var*coeff) %>% 
      dplyr::summarise(y_var = sum(y_var,na.rm = TRUE), model = "ensemble", type = "forecast")
  }
  
  # Generalized function to get forecasts
  
  get_forecast_int <- function(.data, model, horizon, parameter){
    .data_tmp %>% 
      fit_ts(model = model, parameter = parameter) %>% 
      get_forecast(horizon = horizon)
  }
  
  # Replace hyper-parameters on demand
  
  optim_join_int <- function(.data, model, parameter, horizon, best_model){
    if(model %in% c("glmnet", "glm")){ # missing arimax
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
    
    cat(paste0("\nFast optimization for: ", length(model), " Models + Unweighted Ensemble Forecast"))
    
    # Generates forecast given default (hyper)parameters.
    
    forecast_tmp <- map(model, ~get_forecast_int(.data = .data_tmp
                                               , model = .x, horizon = horizon
                                               , parameter = parameter)) %>% 
      bind_rows() %>% 
      dplyr::mutate(y_var_fcst = ifelse(y_var_fcst < 0, 0, y_var_fcst)) %>% 
      dplyr::rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      dplyr::bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      tidyr::replace_na(replace = list(type = "history", model = "history"))
    
    # Ensemble is a simple average of every model's forecast
    
    ensemble_tmp <- get_ensemble_int(.forecast_tmp = forecast_tmp)
    
    forecast_output <- bind_rows(forecast_tmp, ensemble_tmp) %>%
      fill(key, .direction = "down")
    
  } else if(optim_profile == "light"){ # Light profile -------------------------
    
    cat(paste0("\nLight optimization for: ", length(model), " Models + Weighted Ensemble Forecast"))
    
    # slow models
    
    best_model_int <- optim_ts(.data_tmp, test_size = test_size, lag = lag
                               , parameter = parameter, model = model
                               , tune_parallel = tune_parallel
                               , metric = metric)
    
    print(knitr::kable(best_model_int, "simple", 4))
    
    # get n best models
    
    model <- best_model_int %>% 
      dplyr::filter(ranking <= number_best_models) %>% 
      .[["model"]]
    
    forecast_tmp <- map(model
                        , ~ optim_join_int(.data_tmp, model = .x, parameter = parameter
                                                , horizon = horizon, best_model = best_model_int)) %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      dplyr::rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      dplyr::bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      tidyr::replace_na(replace = list(type = "history", model = "history")) 
    
    ensemble_tmp <- get_ensemble_int_weighted(.forecast_tmp = forecast_tmp, .weights = best_model_int[best_model_int$model%in%model,])
    
    forecast_output <- bind_rows(forecast_tmp, ensemble_tmp) %>%
      fill(key, .direction = "down")
    
  }
  
  if(pred_interval == TRUE){ # + Final checks
    
    forecast_output <- get_pred_interval_int(.data_tmp = .data_tmp, .forecast_output = forecast_output)
    forecast_output <- forecast_output %>% 
      mutate(y_var = ifelse(y_var < 0, 0, y_var),
             lower_threshold = ifelse(lower_threshold < 0, 0, lower_threshold),
             upper_threshold = ifelse(upper_threshold < 0, 0, upper_threshold))
    attr(forecast_output, "prescription") <- attributes(.data_tmp)[["prescription"]]
    attr(forecast_output, "output_type") <- "optim_output_pi"
    
  } else { # Main output + Final checks
    
    forecast_output <- forecast_output %>% 
      mutate(y_var = ifelse(y_var < 0, 0, y_var))
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
