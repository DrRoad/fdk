#' Hyperparameter optimization for a glmnet model
#'
#' @param data Data-frame.
#' @param hyperparam List. Hyperparameters to be used for estimation. There are 4 hyperparameters: first, 
#' *alpha* in the space [0,1] controls whether it is a Ridge (L2) a LASSO (L1) shrinkage method, respectively.
#' Any number that lies between is considered as ElasticNet regression, a combination of both regularizations.
#' The other 2 hyperparameters are time weights and trend discount.
#' @param random_sample Numeric. In the space [0,1] defines the Random Search to Grid Search hyperparameter
#' search process, respectively. It is set as 20% random sample by default.
#' @param config List. Configuration that defines the task to be performed.
#'
#' @return
#' @export
#' @import glmnet
#' @import dplyr
#' @import purrr
#' @import furrr
#'
#' @examples
optim_ts <- function(.data, test_size, lag, parameter, model){

  if(model == "glmnet"){
    random_grid <- sample(x = 1:nrow(parameter$glmnet$grid)
                          , size = round(length(1:nrow(parameter$glmnet$grid))*parameter$glmnet$job$random_search_size)
                          , replace = FALSE)
    message(paste0("Estimating ", length(random_grid)* test_size, " models to find the best parameter"))
    
    split_ts(.data, test_size = test_size, lag = lag) %>% 
      enframe(name = "iter", value = "splits") %>% 
      expand_grid(random_grid) %>% 
      transmute(iter,
                map2_dfr(.x = splits, .y = random_grid, .f = ~get_glmnet(.data = .x[["train"]]
                                                                         , parameter = update_parameter(parameter, grid[.y,])) %>% 
                           get_forecast_experimental(x_data = .x[["test"]], tune = TRUE))) %>% 
      mutate(mape_i = abs(.y_var_true - .y_var_pred)/.y_var_true) %>% 
      group_by(trend_discount, time_weight, alpha) %>% 
      summarise(mape = mean(mape_i)
                , lambda_median = median(lambda)
                , lambda_cov = sd(lambda)/mean(lambda)
                , .groups = "drop") %>% 
      arrange(mape, -lambda_cov) %>%
      select(1:3, lambda = lambda_median, cv_mape = mape) %>% 
      .[1:parameter$glmnet$job$n_best_model,]
  } else if(model == "arima") {
    split_ts(.data, test_size = 6, lag = 4) %>% 
      enframe(name = "iter", value = "splits") %>% 
      transmute(map_dfr(.x = splits, .f = ~get_arima_experimental(.data = .x[["train"]]) %>% 
                          get_forecast_experimental(x_data = .x[["test"]], tune = TRUE))) %>% 
      mutate(mape_i = abs(.y_var_true - .y_var_pred)/.y_var_true) %>% 
      group_by(p, d, q, P, D, Q) %>% 
      summarise(mape = mean(mape_i), .groups = "drop") %>% 
      arrange(mape) %>%
      select(1:6, cv_mape = mape) %>% 
      .[1,]
  }
}







train <- l$splits[[1]]$train
x_data <- l$splits[[1]]$test


.fit_output <- get_arima_experimental(train)

get_forecast_experimental(x_data = test)
