# Testing

pkg <- c("glmnet", "lubridate", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly",
         "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW",
         "forecTheta", "prophet", "e1071", "readxl")

invisible(lapply(pkg, require, character.only = TRUE))

# Source ------------------------------------------------------------------

source("R/auxiliar.R")
source("R/data_validation.R")
source("R/data_cleansing.R")
source("R/data_preparation.R")
source("R/feature_engineering.R")
source("R/model_training.R")
source("R/model_tuning.R")
source("R/get_forecast.R")
source("R/autoforecast.R")

# models

source("R/get_croston.R")
source("R/get_seasonal_naive.R")
source("R/get_arima.R")
source("R/get_ets.R")
source("R/get_glm.R")
source("R/get_glmnet.R")
source("R/get_neural_network.R")
source("R/get_tbats.R")
source("R/get_dyn_theta.R")
source("R/get_tslm.R")
source("R/get_prophet.R")
source("R/get_svm.R")

# Parameter ---------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.90, to = 1, by = 0.01)
                           , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1)
                           , alpha = seq(from = 0, to = 1, by = 0.25))
grid_glm <- expand_grid(time_weight = seq(from = 0.90, to = 1, by = 0.01)
                        , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1))

parameter <- list(glmnet = list(time_weight = 1.0, trend_discount = .91, alpha = .7, lambda = 320
                                , grid_glmnet = grid_glmnet
                                , job = list(optim_lambda = FALSE, x_excluded = NULL
                                             , random_search_size = 0.1
                                             , n_best_model = 1))
                  , croston = list(alpha = 0.1)
                  , glm = list(time_weight = 1.0, trend_discount = .91
                               , grid_glm = grid_glm
                               , job = list(x_excluded = NULL
                                            , random_search_size = 0.3
                                            , n_best_model = 1))
                  , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                  , ets = list(ets = "ZZZ"))

# Data import

.data_test <- AirPassengers %>%
  as_tsibble() %>%
  as_tibble() %>%
  mutate(reg_name = "0", reg_value = 0, key = "airpassengers", index = as.Date(index)) %>%
  prescribe_ts(key = "key", y_var = "value", date_var = "index", reg_name = "reg_name", reg_value = "reg_value", freq = 12)

# Prescribe

.data_test_0 <- data_init %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "kalman")

# Single item forecast / modularity ---------------------------------------

## Default parameters

fit_1 <- .data_test_0 %>% # options: winsorize (default), nearest, mean, median. 
  fit_ts(model = "glm", parameter = parameter) %>% 
  get_forecast(horizon = 12)

## Hyperparameter tuning

optim_1 <- .data_test_0 %>% 
  optim_ts(test_size = 6, lag = 3, 
           parameter = parameter, 
           model = "glm")

## Test profiles ------------------------------------------------------------

model <- c("glm", "glmnet", "svm", "prophet", "dyn_theta", "croston", "arima", "ets")

fast_optim_forecast <- autoforecast(.data = .data_test
                                    , horizon = 36
                                    , model = model
                                    , parameter = parameter
                                    , optim_profile = "fast"
                                    , method = "kalman"
                                    , number_best_models = 3
                                    , pred_interval = TRUE)

light_optim_forecast <- autoforecast(.data = .data_test
                                    , horizon = 36
                                    , model = model
                                    , parameter = parameter
                                    , optim_profile = "light"
                                    , method = "kalman"
                                    , number_best_models = 3
                                    , pred_interval = TRUE
                                    , test_size = 3)

### Fast ------------------------------------------------------------

fast_optim_forecast %>% 
  filter(model != "ensemble") %>% 
  plot_ts(interactive = T)

### Light ------------------------------------------------------------

light_optim_forecast %>% 
  filter(model != "ensemble") %>% 
  plot_ts(interactive = T)

#---
