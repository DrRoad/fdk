
#' Fit a Regulalized Generalized Linear Model
#'
#' @param data Data-frame. with response and regressor variables.
#' @param hyperparam List. Hyperparameters to be used for estimation. There are 4 hyperparameters: first, 
#' *alpha* in the space [0,1] controls whether it is a Ridge (L2) a LASSO (L1) shrinkage method, respectively.
#' Any number that lies between is considered as ElasticNet regression, a combination of both regularizations.
#' The other 2 hyperparameters are time weights and trend discount.
#' @param config List. Configuration that defines the task to be performed.
#'
#' @import dplyr
#' @import fastDummies
#' @import glmnet
#' @author Obryan Poyser
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  get_glmnet()
#' }
get_glmnet <- function(data, hyperparam, config){
  time_weights_tmp <- get_time_weights(dep_var = data[,1][[1]] # first columen
                                       , time_weight = hyperparam[["glmnet"]][["time_weight"]])
  # Design matrix
  
  ## Exclude regressorsconfig
  
  all_names <- names(data)
  
  if(is.null(config[["glmnet"]][["excluded_reg"]])==FALSE){
    max_date <- max(data[["date"]])
    data <- data[,!(names(data) %in% config[["glmnet"]][["excluded_reg"]])]
    regressor_names <- names(data)[!(names(data) %in% c("trend", "month", "y"))]
  }
  
  ## Factors to binary
  
  factor_vars <- names(data)[sapply(data, function(x) ifelse(is.character(x) | is.factor (x), T, F))]
  
  ## Data frame to matrix
  
  y_train <- data[,1][[1]] # it is assumed that the first var is the dependent
  x_train <- data[,-1] %>% 
    #mutate_at(.vars = factor_vars, as.factor) %>% 
    dummy_cols(select_columns = factor_vars, remove_first_dummy = T
                            , remove_selected_columns = T) %>% 
    as.matrix()
  
  # Fit
  
  if(config[["glmnet"]][["optim_lambda"]]==FALSE){
    
    fit <- glmnet(x = x_train, y = y_train, weights = time_weights_tmp
                  , alpha = hyperparam[["glmnet"]][["alpha"]]
                  , lambda = hyperparam[["glmnet"]][["lambda"]])
    
    return(list(model_summary = list(train_size = length(data[,1][[1]])
                                     , train_pred = as.vector(predict(fit, newx = x_train))
                                     , max_date = max_date
                                     , x_names = colnames(x_train)
                                     , regressor_names = regressor_names
                                     , factor_vars = factor_vars)
                , config = config
                , param = list(alpha = hyperparam[["glmnet"]][["alpha"]]
                               , lambda = hyperparam[["glmnet"]][["lambda"]]
                               , time_weights = time_weights_tmp
                               , trend_discount = hyperparam[["glmnet"]][["trend_discount"]])
                , fit = fit))
    
  } else if(config[["glmnet"]][["optim_lambda"]]==TRUE){
    
    fit_tmp <- cv.glmnet(x = x_train, y = y_train
                         , alpha = hyperparam_grid[["glmnet"]][["alpha"]]
                         , weights = time_weights_tmp
                         , type.measure = "mae")
    
    fit <- glmnet(x = x_train, y = y_train
                  , weights = time_weights_tmp
                  , alpha = hyperparam[["glmnet"]][["alpha"]]
                  , lambda = fit_tmp$lambda.min)
    
    return(list(model_summary = list(train_size = length(data[,1][[1]])
                                     , train_pred = as.vector(predict(fit, newx = x_train))
                                     , max_date = max_date
                                     , regressor_names = regressor_names
                                     , x_names = colnames(x_train)
                                     , factor_vars = factor_vars)
                , config = config
                , param = list(alpha = hyperparam[["glmnet"]][["alpha"]]
                               , lambda = fit_tmp$lambda.min
                               , time_weights = time_weights_tmp
                               , trend_discount = hyperparam[["glmnet"]][["trend_discount"]])
                , fit = fit))
  }
}

# Testing -----------------------------------------------------------------

# hyperparam <- list(glmnet = list(time_weight = 0.9, trend_discount = .9, alpha = 0, lambda = 0))
# config <- list(lag = 4, glmnet = list(excluded_reg = c("date"), optim_lambda = FALSE))
# 
# demo_data <- read_rds("../demo_data.rds") %>%
#   mutate(month = months(date, abbr = T) %>% as.factor()
#          , trend = 1:n()) %>% 
#   select(y = volume, date, trend, month, reg=oos)
# 
# splits <- ts_split(data = demo_data, test_size = 6, lag = 4)
# 
# train <- splits[[1]]$train
# test <- splits[[1]]$test
# 
# # map(splits, ~enframe(.x) %>% pivot_wider) %>% 
# #   bind_rows(.id = "iter")
# 
# fit_1 <- train %>% 
#   get_glmnet(data = ., hyperparam = list(glmnet = hyperparam[[1]]), config = config) %>% 
#   get_forecast(fit_output = ., test = test)

# Multiple ----------------------------------------------------------------

# demo_data %>% 
#   ts_split(data = ., test_size = 6, lag = 4) %>% 
#   map(.x = ., .f = get_glmnet(data = .x[["train"]], hyperparam = hyperparam, job_config = config) %>% 
#         get_fcst(fit_output = ., test = .x[["test"]]))

#---
