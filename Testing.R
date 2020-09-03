
# Testing

pkg <- c("glmnet", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly",
         "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW",
         "forecTheta")

lapply(pkg, require, character.only = TRUE)

# Source ------------------------------------------------------------------

source("R/get_seasonal_naive.R")
source("R/get_forecast.R")
source("R/get_ets.R")
source("R/get_arima.R")
source("R/get_glm.R")
source("R/get_glmnet.R")
source("R/get_croston.R")
source("R/get_neural_network.R")
source("R/get_tbats.R")
source("R/get_dyn_theta.R")
source("R/get_prophet.R")
source("R/get_tslm.R")
source("R/cleansing.R")
source("R/auxiliar.R")
source("R/autoforecast.R")
source("R/feature_engineering.R")
source("R/optim_ts.R")

# Parameter ---------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.9, to = 1, by = 0.02)
                           , trend_discount = seq(from = 0.95, to = 1, by = 0.01)
                           , alpha = seq(from = 0, to = 1, by = 0.10))
grid_glm <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.02)
                        , trend_discount = seq(from = 0.8, to = 1, by = 0.02))

parameter <- list(glmnet = list(time_weight = .94, trend_discount = .70, alpha = 0, lambda = .1
                                , grid_glmnet = grid_glmnet
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.05
                                             , n_best_model = 1))
                  , croston = list(alpha = 0.1)
                  , glm = list(time_weight = .99, trend_discount = 0.70
                               , grid_glm = grid_glm
                               , job = list(x_excluded = NULL
                                            , random_search_size = 0.1
                                            , n_best_model = 1))
                  , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                  , ets = list(ets = "ZZZ"))

# Data import -------------------------------------------------------------

data_init <- read_csv("demo_data.csv") %>% 
  dplyr::filter(date < "2020-02-01")

# 
# ap0 <- AirPassengers %>%
#   as_tsibble() %>%
#   as_tibble() %>%
#   mutate(key = "airpassenger", reg_name = "0", reg_value = 0
#          , index = as.Date(yearmonth(index))) %>%
#   select(key, date=index, value, reg_name, reg_value)
#
# ap <- prescribe_ts(.data = ap0, key = "key", y_var = "value", date_var = "date"
#                    , reg_name = "reg_name", reg_value = "reg_value", freq = 12)
#

## Every data to be autoforecasted should "prescribed" first to allow attribute inheritance.

data_all <- data_init %>%
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value")

# Single item forecast / modularity ---------------------------------------

### Default parameters

fit_1 <- data_all %>% 
  filter(key == "FI: 515188") %>%
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "winsorize") %>% # options: winsorize (default), nearest, mean, median. 
  fit_ts(model = "dynamic_theta", parameter = parameter)

### Fcst

fit_1 %>% 
  get_forecast(horizon = 36)

### Fit output

attributes(data_all) %>% 
  str()

### Hyperparameter tuning

data_all %>% 
  filter(key == "FI: 515188") %>% 
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "kalman") %>% # options: winsorize (default), nearest, mean, median. 
  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "dynamic_theta")

## Optimization ------------------------------------------------------------

optim_profile <- c("fast", "light") # fast = default parameter, light = small random search

model_list <- c("glm", "glmnet", "neural_network", "arima", "ets",
                "seasonal_naive", "croston", "tbats", "dynamic_theta",
                "tslm")

model_list <- c("prophet")

## Fast

.data <- data_all %>% 
  dplyr::filter(key == "FI: 515188") #%>% 

fast_optim_forecast <- autoforecast(.data = .data
                                    , horizon = 36
                                    , model = model_list
                                    , parameter = parameter
                                    , optim_profile = "fast"
                                    , method = "kalman")
fast_optim_forecast %>% 
  plot_ts(interactive = F)

## Light

light_optim_forecast <- autoforecast(.data = .data
                                     , horizon = 100
                                     , model = model_list
                                     , parameter = parameter
                                     , optim_profile = "light"
                                     , test_size = 6
                                     , lag = 3
                                     , meta_data = FALSE
                                     , tune_parallel = FALSE
                                     , method = "winsorize") # since meta_data = T, a list will be printed.

light_optim_forecast %>% 
  plot_ts(interactive = T)

# Multiple items / Parallel ----------------------------------------------------------

library(foreach)
library(doSNOW)
library(snow)

cluster = makeCluster(4, type = "SOCK")
registerDoSNOW(cluster)
ntasks <- length(unique(data_all$key)[1:5])
progress <- function(n) {
  cat(sprintf(" %d Keys(s) / %.2f%% percent remaining\n",ntasks-n,(ntasks-n)*100/ntasks))
}
opts <- list(progress=progress)

tictoc::tic()
results <- foreach(key_i = unique(data_all$key)[1:5], .combine = "rbind", .options.snow=opts, .packages=c("autoforecast")) %dopar% {
  data_i <- data_all[data_all$key == key_i,]
  autoforecast(.data = data_i, horizon = 100
               , model = model_list
               , parameter = parameter, optim_profile = "light", test_size = 6
               , lag = 3, meta_data = FALSE, method = "winsorize", tune_parallel = FALSE)
}
tictoc::toc()

stopCluster(cl)

### ----------------------------------------------------------

future_map(unique(data_all$key)[1:2], ~.f = {
  data_i <- data_all[data_all$key == .x,]
  autoforecast(.data = data_i, horizon = 100
               , model = model_list
               , parameter = parameter, optim_profile = "fast", test_size = 6
               , lag = 3, meta_data = FALSE, method = "kalman")
})

## Plot

results %>% 
  plot_ts(multiple_keys = T, interactive = T)

#---
