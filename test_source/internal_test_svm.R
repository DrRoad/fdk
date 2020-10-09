# Testing

pkg <- c("glmnet", "lubridate", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly",
         "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW",
         "forecTheta", "prophet", "e1071", "readxl")

invisible(lapply(pkg, require, character.only = TRUE))

# Source ------------------------------------------------------------------

source("R/auxiliar.R")
source("R/cleansing.R")
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

grid_glmnet <- expand_grid(time_weight = seq(from = 0.9, to = 1, by = 0.02)
                           , trend_discount = seq(from = 0.95, to = 1, by = 0.01)
                           , alpha = seq(from = 0, to = 1, by = 0.10))
grid_glm <- expand_grid(time_weight = seq(from = 0.9, to = 1, by = 0.02)
                        , trend_discount = seq(from = 0.9, to = 1, by = 0.02))

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

data_init <- read_csv("test_source/demo_data.csv") %>% 
  dplyr::filter(date < "2020-02-01", forecast_item != "FI: 34142") 

# Prescribe

.data <- data_init %>% 
  filter(forecast_item == "SE: 492598") %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize")

# Single item forecast / modularity ---------------------------------------

## Default parameters

fit_1 <- .data %>% # options: winsorize (default), nearest, mean, median. 
  fit_ts(model = "svm", parameter = parameter) %>% 
  get_forecast(horizon = 12)

## Hyperparameter tuning

optim_1 <- .data %>% 
  optim_ts(test_size = 6, lag = 3, 
           parameter = parameter, 
           model = "svm")

# Optimization ------------------------------------------------------------

data_test <- data_init %>% 
  filter(forecast_item == "SE: 492598") %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value")

## Test profiles ------------------------------------------------------------

model <- "svm"

fast_optim_forecast <- autoforecast(.data = data_test
                                    , horizon = 36
                                    , model = model
                                    , parameter = parameter
                                    , optim_profile = "fast"
                                    , method = "winsorize"
                                    , number_best_models = 3
                                    , pred_interval = FALSE)

light_optim_forecast <- autoforecast(.data = data_test
                                    , horizon = 36
                                    , model = model
                                    , parameter = parameter
                                    , optim_profile = "light"
                                    , method = "winsorize"
                                    , number_best_models = 3
                                    , pred_interval = FALSE)

### Fast ------------------------------------------------------------

light_optim_forecast %>% 
  plot_ts(interactive = T)

# fast_optim_forecast %>% 
#   clipr::write_clip()

### Light ------------------------------------------------------------

light_optim_forecast %>% 
  plot_ts(interactive = T)

# light_optim_forecast %>% 
#   clipr::write_clip()

#---
