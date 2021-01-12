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
#' @param tune_parallel Logical. Perform parallelization across different model selection (**experimental**).
#' @param number_best_models Integer. Among the best models, how many to output (also controls the Ensemble forecast).
#' @param pred_interval Logical. Control if prediction intervals are printed.
#' @param ... Other parameter from the sub-functions.
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
                         , meta_data = FALSE, tune_parallel = TRUE, number_best_models = 1
                         , pred_interval = TRUE, metric = "mape", method = "kalman"
                         , frequency = 12, ensemble = FALSE, ...){
  
  # Internal lag calculation
  
  lag <- lag + 1
  
  # Set seed
  
  set.seed(123)
  
  # Checks -> If there is only one model for output, don't do ensemble
  
  if(length(model) == 1 | number_best_models == 1){
    ensemble <- FALSE
  } 
  
  # Only producte pred interval for best model
  
  if(number_best_models > 1){
    pred_interval <- FALSE
  } 
  
  # Config for fast mode
  
  if(optim_profile == "fast" && length(model) > 1){
    pred_interval <- FALSE
  } 
  
  # Get default parameters
  
  if(is.null(parameter)){
    
    # Parameters -------------------------------------------------------------
    
    grid_glmnet <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.025)
                               , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1)
                               , alpha = seq(from = 0, to = 1, by = 0.25))
    grid_glm <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.025)
                            , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1))
    
    # Parameter list -------------------------------------------------------------
    
    parameter <- list(glmnet = list(time_weight = .95, trend_discount = .70, alpha = 0, lambda = .1
                                    , grid_glmnet = grid_glmnet
                                    , job = list(optim_lambda = FALSE, x_excluded = NULL
                                                 , random_search_size = 0.3
                                                 , n_best_model = 1))
                      , croston = list(alpha = 0.1)
                      , glm = list(time_weight = .99, trend_discount = 0.70
                                   , grid_glm = grid_glm
                                   , job = list(x_excluded = NULL
                                                , random_search_size = 0.3
                                                , n_best_model = 1))
                      , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                      , ets = list(ets = "ZZZ"))
    
  }
  
  # Main validation
  
  .data_tmp <- .data %>% validate_ts()
  
  # Validation attributes
  
  .main_attributes <- attributes(.data_tmp)

  # Main rules of forecasting rules & integrity
  
  if(.main_attributes$prescription$size - test_size < 12 | .main_attributes$prescription$intermittency > 0.5){ # Use simple models
    .data_tmp <- .data_tmp %>% 
      feature_engineering_ts()
    model <- model[model %in% c("arima","ets","croston")]
    cat("Models to be tested:", model)
     if(.main_attributes$prescription$size < 12){ # Super simple: Default croston + fast mode
     .data_tmp <- .data_tmp %>% 
       feature_engineering_ts()
     optim_profile <- "fast"
     model <- c("croston")
     cat("Models to be tested:", model)
     }
  }else{ # Check data in test or 0 consistency
    quantity_test_size <- sum(.data_tmp[["y_var"]][(.main_attributes$prescription$size-test_size):(.main_attributes$prescription$size)])
    if(sum(.data_tmp$y_var) == 0 | quantity_test_size == 0){ # If no data consitency, do feature engineering & select a croston
      .data_tmp <- .data_tmp %>% 
        feature_engineering_ts()
      optim_profile <- "fast"
      model <- "croston"
      cat("Models to be tested:", model)
    }else{ # Else, do feature engineering & cleansing with selected inputs
      .data_tmp <- .data_tmp  %>% 
        feature_engineering_ts() %>% 
        clean_ts(method = method) %>% 
        dplyr::mutate(
          y_var = ifelse(y_var < 0, 0, y_var)
        )
      # Print
      cat("\nProcedures applied: \n- Feature engineering \n- Cleansing\n")
      cat("Models to be tested:", model)
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
      dplyr::filter(type != "history") %>% # Top n models cv
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
    
    # Calculate new forecast
    
    .forecast_tmp_weighted <- .forecast_tmp_ratios %>% 
      dplyr::group_by(model) %>% 
      dplyr::mutate(ind_metric = 1/abs(cv_metric)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(all_metric = sum(unique(ind_metric),na.rm = TRUE),
             coeff = ind_metric/all_metric) 
    
    # Output
    
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
    
    # Generates forecast given default (hyper)parameters.
    
    forecast_tmp <- map(model, ~get_forecast_int(.data = .data_tmp
                                               , model = .x
                                               , horizon = horizon
                                               , parameter = parameter)) %>% 
      bind_rows() %>% 
      dplyr::mutate(y_var_fcst = ifelse(y_var_fcst < 0, 0, y_var_fcst)) %>% 
      dplyr::rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      dplyr::bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      tidyr::replace_na(replace = list(type = "history", model = "history"))
    
    # Ensemble is a simple average of every model's forecast
    
    if(ensemble == TRUE){
      
      cat(paste0("\nFast optimization for: ", length(model), " Models + Unweighted Ensemble Forecast"))
      
      ensemble_tmp <- get_ensemble_int(.forecast_tmp = forecast_tmp)
      
      forecast_output <- bind_rows(forecast_tmp, ensemble_tmp) %>%
        fill(key, .direction = "down")
      
    } else{
      
      cat(paste0("\nFast optimization for ", length(model)),"models")
      
      forecast_output <- forecast_tmp %>%
        fill(key, .direction = "down")
      
    }
    
  } else if(optim_profile == "light"){ # Light profile -------------------------
    
    best_model_int <- optim_ts(.data_tmp, test_size = test_size, lag = lag
                               , parameter = parameter, model = model
                               , tune_parallel = tune_parallel
                               , metric = metric)
    
    print(knitr::kable(best_model_int, "simple", 4))
    
    # Get "n" best models
    
    model <- best_model_int %>% 
      dplyr::filter(ranking <= number_best_models) %>% 
      .[["model"]]
    
    # Forecasting
    
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
    
    if(ensemble == TRUE){
      
      cat(paste0("\nLight optimization for ", length(model), " models + Unweighted Ensemble Forecast"))
      
      ensemble_tmp <- get_ensemble_int_weighted(.forecast_tmp = forecast_tmp, .weights = best_model_int[best_model_int$model%in%model,])
      
      forecast_output <- bind_rows(forecast_tmp, ensemble_tmp) %>%
        fill(key, .direction = "down")
      
    } else{
      
      cat(paste0("\nLight optimization for ", length(model)),"models")
      
      forecast_output <- forecast_tmp %>%
        fill(key, .direction = "down")
      
    }
    
  } else if (optim_profile == "complete"){ # Complete profile -------------------------
    
    # Set parameters to "full power"
    
    parameter$glm$job$random_search_size <- 1
    parameter$glmnet$job$random_search_size <- 1
    parameter$glmnet$job$optim_lambda <- TRUE
    
    best_model_int <- optim_ts(.data_tmp, test_size = test_size, lag = lag
                               , parameter = parameter, model = model
                               , tune_parallel = tune_parallel
                               , metric = metric)
    
    print(knitr::kable(best_model_int, "simple", 4))
    
    # Get arranged best models
    
    model <- best_model_int %>% 
      .[["model"]]
    
    # Train & valid strategy
    
    .data_tmp_train <- .data_tmp[1:(nrow(.data_tmp)-test_size),]
    .data_tmp_valid <- .data_tmp[(nrow(.data_tmp)-test_size+1):nrow(.data_tmp),]
    
    # Forecasting test size
    
    forecast_tmp_0 <- map(model
                        , ~ optim_join_int(.data_tmp_train, model = .x, parameter = parameter
                                           , horizon = test_size, best_model = best_model_int)) %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      dplyr::rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      dplyr::bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      tidyr::replace_na(replace = list(type = "history", model = "history")) %>% 
      dplyr::filter(type == "forecast") %>% 
      dplyr::mutate(key == unique(.data_tmp_train$key)[1])
    
    # Find best test models and forecast again
    
    final_evaluation <- forecast_tmp_0 %>% 
      dplyr::group_by(model) %>% 
      dplyr::summarise(sum_predicted = sum(y_var)) %>% 
      dplyr::mutate(final_mape = abs(sum_predicted-sum(.data_tmp_valid$y_var))/sum(.data_tmp_valid$y_var)) %>% 
      dplyr::arrange(final_mape) %>% 
      dplyr::mutate(ranking = c(1:length(model))) %>% 
      dplyr::select(ranking, model, final_mape)
    
    # Wrangling 
    
    best_model_int_0 <- best_model_int %>% 
      dplyr::select(model, parameter)
    
    # Get arranged best models
    
    best_model_int_final <- final_evaluation %>% 
      dplyr::filter(ranking <= number_best_models) %>% 
      dplyr::left_join(best_model_int_0, by = "model")
    
    model <- best_model_int_final %>% 
      dplyr::filter(ranking <= number_best_models) %>% 
      .[["model"]]
    
    print(knitr::kable(final_evaluation, "simple", 4))
    
    # Final forecasting
    
    forecast_tmp <- map(model
                        , ~ optim_join_int(.data_tmp, model = .x, parameter = parameter
                                           , horizon = horizon, best_model = best_model_int_final)) %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(y_var_fcst = ifelse(y_var_fcst<0, 0, y_var_fcst)) %>% 
      dplyr::rename(date_var = date, y_var = y_var_fcst) %>% 
      dplyr::mutate(type = "forecast") %>% 
      dplyr::bind_rows(.data_tmp, .) %>% 
      dplyr::select(key, date_var, y_var, model, type) %>% 
      tidyr::replace_na(replace = list(type = "history", model = "history")) 
    
    # Ensemble & output
    
    if(ensemble == TRUE){
      
      cat(paste0("\n Complete optimization for ", length(model), " models + Unweighted Ensemble Forecast"))
      
      best_model_int_sup <- best_model_int_final %>% rename("cv_metric" = "final_metric")
      
      ensemble_tmp <- get_ensemble_int_weighted(.forecast_tmp = forecast_tmp, .weights = best_model_int_sup[best_model_int_sup$model%in%model,])
      
      forecast_output <- bind_rows(forecast_tmp, ensemble_tmp) %>%
        fill(key, .direction = "down")
      
    } else{
      
      cat(paste0("\nComplete optimization for ", length(model)),"models")
      
      forecast_output <- forecast_tmp %>%
        fill(key, .direction = "down")
      
    }
    
  }
  
  # Pred intervals & output
  
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
  
  # Light output
  
  if(meta_data == TRUE & optim_profile == "light"){
    
    return(list(forecast_output = forecast_output, ranking = best_model_int))
    
  } else {
    
    return(forecast_output)
    
  }
  
  if(meta_data == TRUE & optim_profile == "complete"){
    
    return(list(forecast_output = forecast_output, ranking = best_model_int_final))
    
  } else {
    
    return(forecast_output)
    
  }
  
}

#---
