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
optim_glmnet <- function(.data, hyperparam, random_sample = .2, config){
  optim_glmnet_int <- function(hyperparam_i, splits){
    tmp <- splits[["train"]] %>% 
      get_glmnet(.data = ., hyperparam = list(glmnet = hyperparam_i), config = config) %>% 
      get_forecast(fit_output = ., test = splits[["test"]], inherited_details = TRUE)
    
    tmp[["iter"]] <- splits[["pars"]][["iter"]]
    
    return(tibble(lambda = round(tmp$fit_output$param$lambda, 1)
                  , y_true = tmp$y_true
                  , y_pred = round(tmp$y_pred, 1)
                  , iter = tmp$iter))
  }
  
  expand_grid(hyperparam = hyperparam[sample(length(hyperparam), size = round(length(hyperparam)*random_sample))]
              , splits = ts_split(.data = data, test_size = 6, lag = config[["lag"]])) %>% 
    mutate(pred = map2(.x = hyperparam, .y = splits
                       , .f = ~optim_glmnet_int(hyperparam = .x, splits = .y))) %>% 
    select(-splits) %>% 
    unnest(c(hyperparam, pred)) %>% 
    group_by(time_weight, trend_discount, alpha) %>% 
    mutate(error = abs(y_true-y_pred)) %>% 
    summarise(mape = sum(error)/sum(y_true)
              , lambda_mean = mean(lambda)
              , lambda_median = median(lambda)
              , lambda_last = last(lambda)
              , lambda_sd = sd(lambda), .groups = "drop") %>%
    top_n(1, wt = -mape) %>% 
    select(mape = mape, time_weight, trend_discount, alpha, lambda = lambda_median)
}







tune_model <- function(.data)








# Testing optimization

# hyperparam_list <- expand_grid(
#   time_weight = seq(from = .9, to = 1, by = .02)
#   , trend_discount = seq(from = .9, to = 1, by = .02)
#   , alpha = seq(from = 0, to = 1, by = .1)
# ) %>% group_nest(comb = 1:n()) %>% pull(data)
# 
# items <- c("SE: 198026", "DK: 578280", "DK: 688222")
# 
# config <- list(lag = 4, glmnet = list(excluded_reg = c("date"), optim_lambda = TRUE))
# config2 <- list(lag = 4, glmnet = list(excluded_reg = c("date"), optim_lambda = FALSE))
# 
# # Testing
# # items <- c("SE: 198026", "DK: 578280", "DK: 688222")
# # hyperparam <- list(glmnet = list(time_weight = 0.9, trend_discount = .9, alpha = 0, lambda = 0))
# # config <- list(lag = 4, glmnet = list(excluded_reg = c("date"), optim_lambda = FALSE))
# 
# demo <- read_rds("../demo_data_multi.rds") %>% 
#   filter(forecast_item %in% items, date< "2020-07-01") %>%
#   pivot_wider(names_from = reg_name, values_from = reg_value) %>% 
#   select(-`0`) %>% 
#   mutate_at(.vars = vars(4:last_col()), ~ifelse(is.na(.x), 0, .x)) %>% 
#   group_by(forecast_item) %>% 
#   filter(cumsum(volume)>0) %>% 
#   ungroup() %>% 
#   janitor::clean_names() %>% 
#   group_nest(forecast_item) %>% 
#   mutate(data = map2(forecast_item, data, ~.y %>% 
#                        select(date, y = volume, matches(janitor::make_clean_names(.x))) %>% 
#                        mutate(trend = 1:n()
#                               , month = as.factor(months(date, abbr = T))) %>% 
#                        select(y, everything())))
# 
# 
# 
# # Example -----------------------------------------------------------------
# 
# op_1 <- optim_glmnet(data = demo$data[[3]], hyperparam = hyperparam_list, random_sample = .2, config = config)
# fit_output <- get_glmnet(data = demo$data[[3]], hyperparam = list(glmnet = op_1[, 2:5]), config = config)
# get_forecast(fit_output, horizon = 60)

#---
