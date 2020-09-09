
# Testing

pkg <- c("glmnet", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly",
         "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW",
         "forecTheta")

lapply(pkg, require, character.only = TRUE)

# Source ------------------------------------------------------------------

# source("R/get_seasonal_naive.R")
# source("R/get_forecast.R")
# source("R/get_ets.R")
# source("R/get_arima.R")
# source("R/get_glm.R")
# source("R/get_glmnet.R")
# source("R/get_croston.R")
# source("R/get_neural_network.R")
# source("R/get_tbats.R")
# source("R/get_dyn_theta.R")
# source("R/get_prophet.R")
# source("R/get_tslm.R")
# source("R/cleansing.R")
# source("R/auxiliar.R")
# source("R/autoforecast.R")
# source("R/feature_engineering.R")
# source("R/optim_ts.R")

# Parameter ---------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.9, to = 1, by = 0.02)
                           , trend_discount = seq(from = 0.95, to = 1, by = 0.01)
                           , alpha = seq(from = 0, to = 1, by = 0.10))
grid_glm <- expand_grid(time_weight = seq(from = 0.85, to = 1, by = 0.02)
                        , trend_discount = seq(from = 0.90, to = 1, by = 0.02))

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

data_init <- read_csv("../hexyon_all.csv")
data_init <- data_init[,-1]

# Every data to be autoforecasted should "prescribed" first to allow attribute inheritance.

data_all <- data_init %>%
  prescribe_ts(key = "key", y_var = "y_var", date_var = "date_var"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value")

# data_all %>% select(key) %>% attributes()

# Single item forecast / modularity ---------------------------------------

# Default parameters

fit_1 <- data_all %>% 
  filter(key == "hexyon_vol") %>%
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "winsorize") %>% # options: winsorize (default), nearest, mean, median. 
  fit_ts(model = "tslm", parameter = parameter)

# Fcst

fit_1 %>% 
  get_forecast(horizon = 36)

# Fit output

attributes(data_all) %>% 
  str()

# Hyperparameter tuning

data_all %>% 
  filter(key == "hexyon_vol") %>% 
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "kalman") %>% # options: winsorize (default), nearest, mean, median. 
  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "tslm")

# Optimization ------------------------------------------------------------

optim_profile <- c("fast", "light") # fast = default parameter, light = small random search

model_list <- c("glm", "glmnet", "arima", "ets", "dynamic_theta", "seasonal_naive", "croston")

## Fast

.data <- data_all %>% 
  dplyr::filter(key == "hexyon_vol") #%>% 

fast_optim_forecast <- autoforecast(.data = .data
                                    , horizon = 24
                                    , model = model_list
                                    , parameter = parameter
                                    , optim_profile = "fast"
                                    , method = "winsorize"
                                    , ensemble = FALSE)

fast_optim_forecast %>% 
  plot_ts(interactive = F)

# Light

model_list <- c("glm", "glmnet", "arima", "ets", "dynamic_theta", "seasonal_naive", "croston")

light_optim_forecast <- autoforecast(.data = .data
                                     , horizon = 24
                                     , model = model_list
                                     , parameter = parameter
                                     , optim_profile = "light"
                                     , test_size = 6
                                     , lag = 3
                                     , meta_data = FALSE
                                     , tune_parallel = FALSE
                                     , method = "winsorize"
                                     , number_best_models = 3
                                     , ensemble = FALSE)

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
  autoforecast(.data = data_i, horizon = 24
               , model = model_list
               , parameter = parameter, optim_profile = "light", test_size = 6
               , lag = 3, meta_data = FALSE, method = "winsorize", tune_parallel = FALSE)
}
tictoc::toc()

stopCluster(cl)

## Plot

# results %>% 
#   plot_ts(multiple_keys = T, interactive = T)

#---
