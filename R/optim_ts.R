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
#' @param parallel Logical. Perform parallelization across different model selection (**experimental**).
#'
#' @importFrom rlang .data
#' @import glmnet
#' @import forecast
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @import furrr
#' 
#' @return
#' @export
#'
#' @examples
optim_ts <- function(.data, test_size, lag, parameter, model, parallel = FALSE){
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
                            , ~get_glmnet(.data = .x[["train"]]
                                          , parameter = update_parameter(old_parameter = parameter
                                                                         , new_parameter = parameter$glmnet$grid[.y, ]
                                                                         , model = "glmnet")) %>% 
                              get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true) %>% 
        group_by(trend_discount, time_weight, alpha) %>% 
        summarise(cv_mape = mean(mape_i)
                  , lambda_median = median(lambda)
                  , lambda_cov = sd(lambda)/mean(lambda)
                  , .groups = "drop") %>% 
        arrange(cv_mape, -lambda_cov)
      
      tibble(model = "glmnet"
             , cv_mape = splits_tmp_cv$cv_mape[1]
             , parameter = list(select(splits_tmp_cv, trend_discount, time_weight
                                       , alpha, lambda = lambda_median) %>% 
                                  slice(1)))
      
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
            mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true) %>%
            select_if(names(.) %in% c("y_var_true", "y_var_fcst", "p"
                                      , "d", "q", "P", "D", "Q", "mape_i"))
          
          tibble(model = "arima", cv_mape = mean(splits_tmp_cv$mape_i)
                 , parameter = list(select(splits_tmp_cv, -y_var_true, -y_var_fcst
                                           , -mape_i) %>% 
                                      slice(n())))
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
                            , ~get_glm(.data = .x[["train"]]
                                          , parameter = update_parameter(old_parameter = parameter
                                                                         , new_parameter = parameter[["glm"]][["grid_glm"]][.y, ]
                                                                         , model = "glm")) %>% 
                              get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true) %>% 
        group_by(trend_discount, time_weight) %>%
        summarise(cv_mape = mean(mape_i), .groups = "drop") %>% 
        top_n(n = 1, wt = -cv_mape)
      })
      
      tibble(model = "glm"
             , cv_mape = splits_tmp_cv[["cv_mape"]]
             , parameter = list(select(splits_tmp_cv, trend_discount, time_weight)
                                )
             )
    } else if(model == "croston"){
      cat(paste0("\nCROSTON: Hyperparameter tuning...\n"))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits")
      
      splits_tmp_cv <- map(.x = splits_tmp$splits
                           , .f = ~get_croston(.data = .x[["train"]]) %>% 
                             get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true)
      
      tibble(model = "croston", cv_mape = mean(splits_tmp_cv$mape_i)
             , parameter = list(select(splits_tmp_cv, -y_var_true, -y_var_fcst
                                       , -mape_i) %>% 
                                  slice(n())))
    } else if(model == "tbats"){
      cat(paste0("\nTBATS: Hyperparameter tuning...\n"))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits")
      
      splits_tmp_cv <- map(.x = splits_tmp$splits
                           , .f = ~get_tbats(.data = .x[["train"]]) %>% 
                             get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true)
      
      tibble(model = "tbats", cv_mape = mean(splits_tmp_cv$mape_i)
             , parameter = list(select(splits_tmp_cv, -y_var_true, -y_var_fcst
                                       , -mape_i) %>% 
                                  slice(n())))
    } else if(model == "seasonal_naive"){
      cat(paste0("\nSEASONAL NAIVE: Hyperparameter tuning...\n"))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits")
      
      splits_tmp_cv <- map(.x = splits_tmp$splits
                           , .f = ~get_seasonal_naive(.data = .x[["train"]]) %>% 
                             get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true)
      
      tibble(model = "seasonal_naive", cv_mape = mean(splits_tmp_cv$mape_i)
             , parameter = list(select(splits_tmp_cv, -y_var_true, -y_var_fcst
                                       , -mape_i) %>% 
                                  slice(n())))
    } else if(model == "ets"){
      cat(paste0("\nETS: Hyperparameter tuning...\n"))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits")
      
      splits_tmp_cv <- map(.x = splits_tmp$splits
                           , .f = ~get_ets(.data = .x[["train"]]) %>% 
                             get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true)
      
      tibble(model = "ets", cv_mape = mean(splits_tmp_cv$mape_i)
             , parameter = list(select(splits_tmp_cv, -y_var_true, -y_var_fcst
                                       , -mape_i) %>% 
                                  slice(n())))
    } else if(model == "neural_network"){
      cat(paste0("\nNEURAL NETWORK: Hyperparameter tuning...\n"))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits")
      
      splits_tmp_cv <- map(.x = splits_tmp$splits
                           , .f = ~get_neural_network(.data = .x[["train"]]) %>% 
                             get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true)
      
      tibble(model = "neural_network", cv_mape = mean(splits_tmp_cv$mape_i)
             , parameter = list(select(splits_tmp_cv, -y_var_true, -y_var_fcst
                                       , -mape_i) %>% 
                                  slice(n())))
    }
  } # Close switcher
  
  optim_switcher_safe <- purrr::possibly(optim_switcher, otherwise = NA)
    
  optim_out <- map(model, .f = ~optim_switcher_safe(.x)) %>% 
    bind_rows() %>% 
    arrange(cv_mape) %>% 
    mutate(ranking = 1:n(), .before = "model")
  
  for(i in seq_along(optim_out$model)){
    attr(optim_out[["parameter"]][[i]], "output_type") <- "optim_out"
  }
  return(optim_out)
}
