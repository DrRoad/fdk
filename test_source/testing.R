# Testing

# autoforecast - testing

# If needed: load_pkgs --------------

pkg <- c("autoforecast","glmnet", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly",
         "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW",
         "prophet", "forecTheta")

lapply(pkg, require, character.only = TRUE)

# Parameters -------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.90, to = 1, by = 0.02)
                           , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1)
                           , alpha = seq(from = 0, to = 1, by = 0.25))
grid_glm <- expand_grid(time_weight = seq(from = 0.90, to = 1, by = 0.02)
                        , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1))

# Parameter list -------------------------------------------------------------

parameter <- list(glmnet = list(time_weight = .94, trend_discount = .70, alpha = 0, lambda = .1
                                , grid_glmnet = grid_glmnet
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.5
                                             , n_best_model = 1))
                  , croston = list(alpha = 0.1)
                  , glm = list(time_weight = .99, trend_discount = 0.70
                               , grid_glm = grid_glm
                               , job = list(x_excluded = NULL
                                            , random_search_size = 0.5
                                            , n_best_model = 1))
                  , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                  , ets = list(ets = "ZZZ"))

# Data import

data_init <- read_csv("test_source/demo_data.csv")

data_all <- data_init %>%
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value") %>% 
  feature_engineering_ts()

data_test <- data_all %>% filter(key == unique(data_all$key)[1])

model <- c("glm")

# Testing 1

fit <- data_test %>% 
  fit_ts(model = model, parameter = parameter) %>% 
  get_forecast(horizon = 12, tune = FALSE) 

# Testing 2
  
# fit <- data_test %>% 
#   optim_ts(test_size = 6, lag = 4, parameter = parameter, model = model, tune_parallel = TRUE, metric = "mape") %>% 
#   get_forecast(horizon = 12, tune = FALSE) 

#---
