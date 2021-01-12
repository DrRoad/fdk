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
source("R/plot_ts.R")

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

# Data import -------------------------------------------------------------

.data_test <- AirPassengers %>%
  as_tsibble() %>%
  as_tibble() %>%
  mutate(reg_name = "0", reg_value = 0, key = "airpassengers", index = as.Date(index)) %>%
  prescribe_ts(key = "key", y_var = "value", date_var = "index", reg_name = "reg_name", reg_value = "reg_value", freq = 12)

# Test -------------------------------------------------------------

.data_test_0 <- .data_test %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "kalman")

# Single item forecast / modularity ---------------------------------------

fit_1 <- .data_test_0 %>% # options: winsorize (default), nearest, mean, median. 
  fit_ts(model = "glm", parameter = NULL) %>% 
  get_forecast(horizon = 12)

# Hyperparameter tuning ---------------------------------------

optim_1 <- .data_test_0 %>% 
  optim_ts(test_size = 6, lag = 3, 
           parameter = NULL, 
           model = "glm")

# Test profiles ------------------------------------------------------------

model <- c("glm", "glmnet", "arima", "ets", "croston")

fast_optim_forecast <- autoforecast(.data = .data_test
                                    , horizon = 36
                                    , model = model
                                    , parameter = NULL
                                    , optim_profile = "fast"
                                    , method = "kalman")

light_optim_forecast <- autoforecast(.data = .data_test
                                        , horizon = 36
                                        , model = model
                                        , parameter = NULL
                                        , optim_profile = "light"
                                        , method = "kalman"
                                        , number_best_models = 1
                                        , test_size = 6)

complete_optim_forecast <- autoforecast(.data = .data_test
                                    , horizon = 36
                                    , model = model
                                    , parameter = NULL
                                    , optim_profile = "complete"
                                    , method = "kalman"
                                    , number_best_models = 1
                                    , test_size = 6)

# Fast ------------------------------------------------------------

fast_optim_forecast %>%
  plot_ts(interactive = F)

# Light ------------------------------------------------------------

light_optim_forecast %>%
  plot_ts(interactive = F)

# Complete ------------------------------------------------------------

complete_optim_forecast %>%
  plot_ts(interactive = F)

#---
