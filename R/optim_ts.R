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
  options(warn = -1)
  
  optim_switcher <- function(model){
    if(model == "glmnet"){
      random_grid <- sample(x = 1:nrow(parameter$glmnet$grid)
                            , size = round(length(1:nrow(parameter$glmnet$grid))*parameter$glmnet$job$random_search_size)
                            , replace = FALSE)
      message(paste0("GLMNET: Hyperparameter tuning ", length(random_grid)* test_size, " models..."))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits") %>% 
        expand_grid(random_grid)
      
      splits_tmp_cv <- map2(.x = splits_tmp$splits, .y = splits_tmp$random_grid
                            , ~get_glmnet(.data = .x[["train"]]
                                          , parameter = update_parameter(parameter
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
      random_grid <- sample(x = 1:nrow(parameter$glm$grid)
                            , size = round(length(1:nrow(parameter$glm$grid))*parameter$glm$job$random_search_size)
                            , replace = FALSE)
      message(paste0("GLM: Hyperparameter tuning ", length(random_grid)* test_size, " models..."))
      
      splits_tmp <- split_ts(.data, test_size = test_size, lag = lag) %>% 
        enframe(name = "iter", value = "splits") %>% 
        expand_grid(random_grid)
      
      splits_tmp_cv <- map2(.x = splits_tmp$splits, .y = splits_tmp$random_grid
                            , ~get_glm(.data = .x[["train"]]
                                          , parameter = update_parameter(parameter
                                                                         , new_parameter = parameter$glm$grid[.y, ]
                                                                         , model = "glm")) %>% 
                              get_forecast(x_data = .x[["test"]], tune = TRUE)) %>% 
        bind_rows() %>%
        mutate(mape_i = abs(y_var_true - y_var_fcst)/y_var_true) %>% 
        group_by(trend_discount, time_weight) %>%
        summarise(cv_mape = mean(mape_i), .groups = "drop") %>% 
        arrange(cv_mape)
      
      tibble(model = "glm"
             , cv_mape = splits_tmp_cv$cv_mape[1]
             , parameter = list(select(splits_tmp_cv, trend_discount, time_weight) %>% 
                                  slice(1)))
    } else if(model == "croston"){
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
  

  # Parallelization Experimental

  if(length(model) == 1){
    optim_switcher(model)
  } else if(length(model)> 1){
    if(parallel == TRUE){
      plan(multisession(workers = (parallel::detectCores()-2)))
      future_map(model, .f = ~optim_switcher(.x)) %>% 
        bind_rows() %>% 
        arrange(cv_mape)
    } else if(parallel == FALSE){
      map(model, .f = ~optim_switcher(.x)) %>% 
        bind_rows() %>% 
        arrange(cv_mape)
    }
  }
  
  options(warn = 1)
}
