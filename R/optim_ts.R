#' Hyperparameter optimization for a glmnet model
#'
#' @param .data Data frame or tibble.
#' @param test_size Numeric. How many periods will be use to asses the forecast accuracy.
#' @param lag Numeric. How many periods ahead to start the test size. 
#' @param parameter List. Hyperparameters to be used for estimation. There are 4 hyperparameters: first, 
#' *alpha* in the space [0,1] controls whether it is a Ridge (L2) a LASSO (L1) shrinkage method, respectively.
#' Any number that lies between is considered as ElasticNet regression, a combination of both regularizations.
#' The other 2 hyperparameters are time weights and trend discount.
#' @param model String. Model to be optimized.
#' @param tune_parallel Logical. Perform parallelization across different model selection (**experimental**).
#'
#' @import glmnet
#' @import forecast
#' @import tidyr
#' @import dplyr
#' @import utils
#' @importFrom purrr map
#' @importFrom purrr map2
#' @import foreach
#' 
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' optim_ts()
#' }
optim_ts <- function(.data, test_size, lag, parameter, model, tune_parallel = FALSE){
  #y_var_true <- cv_mape <- ranking <- y_var_fcst <- . <- key <- y_var <- type <- date_var <- NULL
  #globalVariables(c("trend_discount", "time_weight", "lambda", "lambda_cov", "model_i"))
  
  # Find the best parameter among the vector
  ## For strings takes the mode, for numeric average.
  
  best_parameter_int <- function(best_par_string){
    if(is.numeric(best_par_string)== TRUE){
      round(median(best_par_string, na.rm = TRUE), 3)
    } else if(is.character(best_par_string) == TRUE ){
      unique_par <- unlist(unique(best_par_string))
      names(sort(sapply(unique_par, FUN = function(x) sum(x == best_par_string)), decreasing = T))[1]
    } else {
      NULL
    }
  }
  
  # General function for ts based models
  
  split_general_int <- function(model){
    
    splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
      enframe(name = "iter", value = "splits")
    
    map(.x = splits_tmp$splits
        , .f = ~ fit_ts(.data = .x[["train"]], model = model) %>% 
          get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
      bind_rows() %>% 
      summarise(cv_mape = accuracy_metric(y_var_true = sum(y_var_true)
                                          , y_var_pred = sum(y_var_fcst))
                , model = model
                , ranking = NA_integer_
                , parameter = list(best_parameter_int(.[["parameter"]]))
                , .groups = "drop") %>% 
      arrange(cv_mape) %>% 
      select(ranking, model, cv_mape, parameter)
    
  }

  optim_switcher <- function(model){
    if(model == "glmnet"){
      random_grid <- sample(x = 1:nrow(parameter$glmnet$grid)
                            , size = round(length(1:nrow(parameter$glmnet$grid))*parameter$glmnet$job$random_search_size)
                            , replace = FALSE)
      cat(paste0("\nElastic Net: Hyperparameter tuning - Fitting ", length(random_grid) * test_size, " models...\n"))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits") %>% 
        expand_grid(random_grid)
      
      splits_tmp_cv <- map2(.x = splits_tmp$splits, .y = splits_tmp$random_grid
                            , ~fit_ts(.data = .x[["train"]]
                                          , parameter = update_parameter(old_parameter = parameter
                                                                         , new_parameter = parameter$glmnet$grid[.y, ]
                                                                         , model = "glmnet"), model = "glmnet") %>% 
                              get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>% 
        group_by(trend_discount, time_weight, alpha) %>%
        summarise(cv_mape = accuracy_metric(y_var_true = sum(y_var_true)
                                            , y_var_pred = sum(y_var_fcst))
                  , lambda_cov = sd(lambda)/mean(lambda, na.rm = TRUE)
                  , lambda = median(lambda, na.rm = TRUE)
                  , model = "glmnet"
                  , ranking = NA_integer_
                  , .groups = "drop") %>%
        arrange(cv_mape, lambda_cov) %>%
        slice(1) %>%
        transmute(ranking, model, cv_mape, parameter = list(select(., trend_discount
                                                                   , time_weight
                                                                   , alpha
                                                                   , lambda)))
      
    } else if(model == "arima") {
      cat(paste0("\nARIMA: Hyperparameter tuning...\n"))
      
      suppressMessages(
        {
          splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
            enframe(name = "iter", value = "splits")
          
          splits_tmp_cv <- map(.x = splits_tmp$splits
                               , .f = ~get_arima(.data = .x[["train"]]) %>% 
                                 get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
            bind_rows() %>%
            select_if(names(.) %in% c("y_var_true", "y_var_fcst", "p"
                                      , "d", "q", "P", "D", "Q", "mape_i")) %>% 
            bind_rows((tibble(y_var_true = NA, y_var_fcst = NA
                              , p = NA, d = NA, q = NA, P = NA
                              , D = NA, Q = NA) %>% 
                         slice(0)), .) %>% 
            mutate_at(.vars = vars(matches("p$|d$|q$")), ~ifelse(is.na(.x), 0, .x)) %>% 
            summarise(ranking = NA, model = "arima"
                      , cv_mape = accuracy_metric(y_var_true = sum(y_var_true)
                                                  , y_var_pred = sum(y_var_fcst))
                      , parameter = list(select(., 3:last_col()) %>% slice(n())))
        }
      )
      
    } else if(model == "glm"){
      
      random_grid <- sample(x = 1:nrow(parameter$glm$grid_glm)
                            , size = round(length(1:nrow(parameter$glm$grid_glm))*parameter$glm$job$random_search_size)
                            , replace = FALSE)
      cat(paste0("\nGLM: Hyperparameter tuning - Fitting ", length(random_grid) * test_size, " models...\n"))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits") %>% 
        expand_grid(random_grid)
      
      suppressWarnings({
        splits_tmp_cv <- map2(.x = splits_tmp$splits, .y = splits_tmp$random_grid
                              , ~fit_ts(.data = .x[["train"]]
                                        , parameter = update_parameter(old_parameter = parameter
                                                                       , new_parameter = parameter[["glm"]][["grid_glm"]][.y, ]
                                                                       , model = "glm"), model = "glm") %>% 
                                get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        group_by(trend_discount, time_weight) %>% 
        summarise(cv_mape = accuracy_metric(y_var_true = sum(y_var_true)
                                            , y_var_pred = sum(y_var_fcst))
                  , model = "glm"
                  , ranking = NA_integer_
                  , .groups = "drop") %>% 
        arrange(cv_mape) %>%
        slice(1) %>% 
        transmute(ranking, model, cv_mape, parameter = list(select(., trend_discount
                                                                   , time_weight)))
      }
      )
      
    } else if(model == "dynamic_theta") {
      
      cat(paste0("\nDYNAMIC THETA: Tuning...\n"))
      
      suppressMessages(
        {
          splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
            enframe(name = "iter", value = "splits")
          
          map(.x = splits_tmp$splits
              , .f = ~ fit_ts(.data = .x[["train"]], model = model) %>% 
                get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
            bind_rows() %>% 
            summarise(cv_mape = accuracy_metric(y_var_true = sum(y_var_true)
                                                , y_var_pred = sum(y_var_fcst))
                      , model = model
                      , ranking = NA_integer_
                      , parameter = list(NULL)
                      , .groups = "drop") %>% 
            arrange(cv_mape) %>% 
            select(ranking, model, cv_mape, parameter)
        }
      )
      
    } else if(model == "tslm") {
      
      cat(paste0("\nSIMPLE LINEAR MODEL: Tuning...\n"))
      
      suppressMessages(
        {
          splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
            enframe(name = "iter", value = "splits")
          map(.x = splits_tmp$splits
              , .f = ~ fit_ts(.data = .x[["train"]], model = model) %>% 
                get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
            bind_rows() %>% 
            summarise(cv_mape = accuracy_metric(y_var_true = sum(y_var_true)
                                                , y_var_pred = sum(y_var_fcst))
                      , model = model
                      , ranking = NA_integer_
                      , parameter = list(NULL)
                      , .groups = "drop") %>% 
            arrange(cv_mape) %>% 
            select(ranking, model, cv_mape, parameter)
        }
      )
      
    } else if(model == "prophet") {
      
      cat(paste0("\nPROPHET: Tuning...\n"))
      
      suppressMessages(
        {
          splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
            enframe(name = "iter", value = "splits")
          map(.x = splits_tmp$splits
              , .f = ~ fit_ts(.data = .x[["train"]], model = model) %>% 
                get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
            bind_rows() %>% 
            summarise(cv_mape = accuracy_metric(y_var_true = sum(y_var_true)
                                                , y_var_pred = sum(y_var_fcst))
                      , model = model
                      , ranking = NA_integer_
                      , parameter = list(NULL)
                      , .groups = "drop") %>% 
            arrange(cv_mape) %>% 
            select(ranking, model, cv_mape, parameter)
        }
      )
    } else if((model %in% c("croston", "tbats", "seasonal_naive", "ets")) == TRUE){ # Forecast models
      
      cat(paste0("\n", toupper(model), ": Hyperparameter tuning...\n"))
      splits_tmp_cv <- split_general_int(model)
      
    }
  } # Close switcher
  
  # Safe version of the switcher
  
  optim_switcher_safe <- purrr::possibly(optim_switcher, otherwise = NA)
  
  # Nested parallel/cores by model
  
  if(tune_parallel == TRUE){
    optim_out <- foreach(model_i = model, .combine = "rbind") %dopar% {
      optim_switcher(model_i)
      } %>% 
      arrange(cv_mape) %>% 
      mutate(ranking = 1:n(), .before = "model")
  } else if(tune_parallel == FALSE){ # Sequential
    optim_out <- map(model, .f = ~optim_switcher_safe(.x)) %>% 
      bind_rows() %>% 
      arrange(cv_mape) %>% 
      mutate(ranking = 1:n(), .before = "model")
  }
  
  # Sequential
  
  attr(optim_out, "output_type") <- "optim_out"
  
  return(optim_out)
  
}
