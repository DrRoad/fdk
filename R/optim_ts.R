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
optim_ts <- function(.data, test_size, lag, parameter, model){
  
  if(model == "glmnet"){
    random_grid <- sample(x = 1:nrow(parameter$glmnet$grid)
                          , size = round(length(1:nrow(parameter$glmnet$grid))*parameter$glmnet$job$random_search_size)
                          , replace = FALSE)
    message(paste0("Hyperparameter tuning, finding the best combination among ", length(random_grid)* test_size, " models"))
    
    split_ts(.data, test_size = test_size, lag = lag) %>% 
      enframe(name = "iter", value = "splits") %>% 
      expand_grid(random_grid) %>% 
      transmute(map2_dfr(.x = splits, .y = random_grid
                         , .f = ~get_glmnet(.data = .x[["train"]]
                                            , parameter = update_parameter(parameter = parameter
                                                                           , new_parameter = parameter$glmnet$grid[.y, ]
                                                                           , model = "glmnet")
                                            ) %>% 
                           get_forecast(x_data = .x[["test"]], tune = TRUE)
                         )
                ) %>% 
      mutate(mape_i = abs(y_var_true - y_var_pred)/y_var_true) %>% 
      group_by(trend_discount, time_weight, alpha) %>% 
      summarise(mape = mean(mape_i)
                , lambda_median = median(lambda)
                , lambda_cov = sd(lambda)/mean(lambda)
                , .groups = "drop") %>% 
      arrange(mape, -lambda_cov) %>%
      select(1:3, lambda = lambda_median, cv_mape = mape) %>% 
      .[1:parameter$glmnet$job$n_best_model,]
    
  } else if(model == "arima") {
    suppressMessages(
      {
        split_ts(.data, test_size = 6, lag = 4) %>% 
          enframe(name = "iter", value = "splits") %>% 
          transmute(map_dfr(.x = splits, .f = ~get_arima(.data = .x[["train"]]) %>% 
                              get_forecast(x_data = .x[["test"]], tune = TRUE))) %>% 
          mutate(mape_i = abs(y_var_true - y_var_pred)/y_var_true) %>% 
          summarise(p = last(p)
                    , d = last(d)
                    , q = last(q)
                    , P = last(P)
                    , D = last(D)
                    , Q = last(Q), cv_mape = mean(mape_i))
      }
    )
  }
}
