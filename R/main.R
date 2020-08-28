
# Package -----------------------------------------------------------------

library("glmnet")
library("forecast")
library("stlplus")
library("fastDummies")
library("imputeTS")
library("tidyverse")

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
source("R/cleansing.R")
source("R/auxiliar.R")
source("R/autoforecast.R")
source("R/feature_engineering.R")
source("R/optim_ts.R")


# Parameter ---------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.7, to = 1, by = 0.05)
                           , trend_discount = seq(from = 0.9, to = 1, by = 0.05)
                           , alpha = seq(from = 0, to = 1, by = 0.10))
grid_glm <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.02)
                        , trend_discount = seq(from = 0.8, to = 1, by = 0.02))

parameter <- list(glmnet = list(time_weight = .94, trend_discount = .93, alpha = 0, lambda = .1
                                , grid_glmnet = grid_glmnet
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.05
                                             , n_best_model = 1))
                  , croston = list(alpha = 0.1)
                  , glm = list(time_weight = .99, trend_discount = 0.97
                               , grid_glm = grid_glm
                               , job = list(x_excluded = NULL
                                            , random_search_size = 0.1
                                            , n_best_model = 1))
                  , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                  , ets = list(ets = "ZZZ"))

# Data import -------------------------------------------------------------

data_init <- read_csv("demo_data.csv") %>% 
  dplyr::filter(date < "2020-02-01")

## Every data to be autoforecasted should "prescribed" first to allow attribute inheritance.

data_all <- data_init %>%
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value")

# Single item forecast / modularity ---------------------------------------

### Default parameters

fit_1 <- data_all %>% 
  filter(key == "FI: 34142") %>% 
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "winsorize") %>% # options: winsorize (default), nearest, mean, median. 
  #fit_ts(model = "glm", parameter = parameter) %>% 
  get_glm(parameter = parameter) # in v0.1.2 a generalized version is also possible (see line above)

#### Fit output
  summary(fit_1)
  
#### Forecast
fit_1 %>% 
  get_forecast(horizon = 100) # horizon creates a synthetic data, for counterfactual use argument "x_data".

### Hyperparameter tuning

data_all %>% 
  filter(key == "FI: 34142") %>% 
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "kalman") %>% # options: winsorize (default), nearest, mean, median. 
  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = model)


## Optimization ------------------------------------------------------------

optim_profile <- c("fast", "light") # fast = default parameter, light = small random search

model_list <- c("glm", "glmnet", "neural_network", "arima", "ets"
                , "seasonal_naive", "tbats", "croston")

## Fast

fast_optim_forecast <- autoforecast(.data = .data, horizon = 100
             , model = model_list
             , parameter = parameter, optim_profile = "fast", method = "kalman")

fast_optim_forecast %>% 
  plot_ts()

## Light

light_optim_forecast <- autoforecast(.data = .data, horizon = 100
                   , model = model_list
                   , parameter = parameter, optim_profile = "light", test_size = 6
                   , lag = 3, meta_data = T, method = "kalman") # since meta_data = T, a list will be printed.

light_optim_forecast$forecast_output %>% 
  plot_ts()

# Multiple items ----------------------------------------------------------

library(doParallel)
library(foreach)
library(parallel)

cores <- detectCores()
cluster <- makeCluster(2)
  registerDoParallel(cores = 3)

results <- foreach(key_i = unique(data_all$key)[1:2], .combine = "rbind") %dopar% {
  data_i <- data_all[data_all$key == key_i,]
  
  autoforecast(.data = data_i, horizon = 100
               , model = model_list
               , parameter = parameter, optim_profile = "light", test_size = 6
               , lag = 3, meta_data = FALSE, method = "kalman")
}

## Plot

results %>% 
  plot_ts(multiple_keys = T, interactive = T)


.data <- results





foreach(key_i = unique(data_all$key)) %dopar% {
  autoforecast(.data = filter(data_all, key = key_i), horizon = 100
               , model = model_list
               , parameter = parameter, optim_profile = "light", test_size = 6
               , lag = 3, meta_data = T, method = "kalman")
}



.data <- .data %>% 
  feature_engineering_ts() %>% 
  clean_ts() %>% 

.data %>% 
  feature_engineering_ts() %>% 
  clean_ts() %>% 
  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "glm")
  get_glm(parameter = parameter) %>% 
  get_forecast(horizon = 100)




model_list <- "neural_network"


l <- autoforecast(.data = .data, horizon = 100
            , model = model_list
            , parameter = parameter, optim_profile = "fast")

l1 <- autoforecast(.data = .data, horizon = 100
                  , model = model_list
                  , parameter = parameter, optim_profile = "light", test_size = 6, lag = 3, meta_data = T)












l1 %>% 
  print_ts(interactive = T)


tictoc::tic()
autoforecast(.data = .data, horizon = 100
             , model = c("glm", "glmnet", "neural_network", "arima", "ets"
                         , "seasonal_naive", "tbats", "croston")
             , parameter = parameter, optim_profile = "light", test_size = 6, lag = 3)
tictoc::toc()




# Test --------------------------------------------------------------------

optim_light <- .data_1 %>% 
  filter(key == "FI: 34142") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize") %>% 
  optim_ts(test_size = 6, lag = 3, parameter = parameter
           , model = model_list)


fit_ts(.data = .data, parameter = update_parameter(parameter, optim_light$parameter[[1]]
                                                   , model = "glmnet")
       , model = "glmnet") %>% 
  get_forecast(horizon= 100)





best_model <- optim_ts(.data, test_size = 6, lag = 3, parameter = parameter, model = model_list)
best_model_table <- best_model
model <- "glmnet"





optim_join <- function(.data, model, parameter, horizon, best_model){
  if(model %in% c("glmnet", "glm")){ # missing arima
    get_forecast_int(.data, model = model
                     , parameter = update_parameter(parameter
                                                    , best_model_table$parameter[[which(best_model_table$model==model)]]
                                                    , model = model, optim = TRUE)
                     , horizon = 100)
  } else {
    get_forecast_int(.data, model = model, parameter = parameter, horizon = horizon)
  }
}

optim_join(.data, "seasonal_naive", parameter, 100, best_model)



forecast_tmp <- map(model_list, ~optim_join(.data, model = .x, parameter = parameter, horizon = 100, best_model)) %>% 
  bind_rows() %>% 
  rename(date_var = date, y_var = y_var_fcst) %>% 
  mutate(type = "forecast") %>% 
  bind_rows(.data_tmp, .) %>% 
  select(key:y_var, model, type) %>% 
  replace_na(replace = list(type = "history", model = "history"))

ensemble_tmp <- forecast_tmp %>% 
  filter(type != "history") %>% 
  group_by(date_var) %>% 
  summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")

forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp)

attr(forecast_tmp, "output_type") <- "optim_output"


optim_join(.data, model = "glmnet"
           , parameter = parameter
           , horizon = 10)




which(light_optim_best_par$model=="glmnet")


fast_optim_int(.data, model = "glmnet"
               , parameter = update_parameter(parameter
                                              , light_optim_best_par$parameter[[which(light_optim_best_par$model=="glmnet")]]
                                              , "glmnet")
               , horizon = 100)






















new_parameter <- optim_light$parameter[[1]]
old_parameter <- parameter


update_parameter(old_parameter = parameter, new_parameter = new_parameter, model = "glmnet")








.data <- .data_1 %>% 
  filter(key == "FI: 34142")



.data_1 %>% 
  filter(key == "FI: 34142") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize") %>% 
  optim_ts(test_size = 6, lag = 3, parameter = parameter
           , model = model_list)
tictoc::toc()



.forecast <- .data_1 %>% 
  filter(key == "DK: 578281") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize") %>% 
  fit_ts(model = "tbats", parameter = parameter) %>% 
  get_forecast(horizon = 100)


optim_profile <- c("fast", "light", "medium", "complete")



.data <- .data %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize")


.data %>% 
  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "ets")

l %>% 
  filter(type != "history") %>% 
  group_by(date_var) %>% 
  summarise(y_var = mean(y_var), model = "ensemble", type = "forecast")














l1 %>% 
  print_ts(interactive = T)






l <- forecast_ts(.data = .data, horizon = 100
            , model = c("glm", "glmnet", "arima", "ets", "seasonal_naive", "tbats", "croston")
            , parameter = parameter, optim_profile = "fast")






.data <- .data_1 %>% 
  filter(key == "DK: 578281") 







%>% 
  clean_ts(method = "kalman") %>% 
  get_ets() %>% 
  get_forecast(horizon = 100)

.data <- .data_1$data[[1]] %>% 
  clean_ts(method = "kalman") %>% 
  optim_ts(test_size = 6, lag = 3, model = c("arima", "glmnet"), parameter = parameter)


pkg <- c("magrittr", "dplyr", "stlplus", "glmnet", "forecast")


foreach(key=seq_along(.data_1$key), .packages = pkg) %dopar% {
  .data_1$data[[key]] %>% 
    clean_ts(method = "winsorize") %>% 
    get_ets() %>% 
    get_forecast(horizon = 100)
}


function



.data_1$data[[1]] %>% 
  clean_ts(method = c("kalman", "winsorize")) %>% 
  ggplot(aes(date_var, y_var))+
  geom_line()






















.data <- .data$data[[1]]

.data %>% 
  clean_ts() %>% 
  View()


t1$data[[1]] %>% 
  pivot_wider(names_from = "reg_name", values_from = "reg_value") %>% 
  select(-matches("0|NA$")) %>% 
  janitor::clean_names() %>% 
  mutate_at(.vars = vars(-matches("date_var|key|y_var")), .funs = ~ifelse(is.na(.x), 0, .x)) %>% 
  get_design_matrix()





keep_reg <- sum(t1$data[[1]][["reg_value"]])>0

if(keep_reg == FALSE){
  x %>% 
    select(-reg_value, -reg_name)
} else {
  x
}
t1$data[[1]] %>% 
  select_if(sum(reg_value))

sum(unique(t1$data[[1]]$reg_name)!="0")







.data_tmp %>% 
  group_by(key) %>% 
  mutate(reg_name = ifelse(reg_name == 0, NA, reg_name)) %>% 
  summarise(obs = n()
            , n_regressor = n_distinct(na.omit(unique(reg_name)))
            , date_range = paste0(min(date), " / ", max(date))
            , .groups = "drop") %>% 
  knitr::kable(., "simple")


cat(paste0("#", unique(.data_tmp$key)))
