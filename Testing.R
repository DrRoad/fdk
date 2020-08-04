
# autoforecast - testing

# load_pkgs --------------

require(pacman)

pacman::p_load(forecast,tidyverse,seastests,tsfeatures,dplyr,
               lubridate,zoo,DescTools,dvmisc,ggplot2,tsibble,
               prophet,imputeTS,glmnet, tictoc, fastDummies)

# Skus --------------

Skus <- c("SE: 198026", "DK: 578280", "DK: 688222")

# Data --------------

data0 <- readRDS("../demo_data_multi.rds")
data0 <- data0 %>% select(forecast_item, volume, date, reg_value) %>%
  rename(key = forecast_item, y = volume, reg = reg_value)
data0 <- data0 %>% filter(key %in% Skus)
data <- data0 %>% filter(date < "2020-07-01") %>% arrange(key,date)

# Parameters --------------

parameters <- list(params_arima = NULL, params_croston = NULL,
                   glmnet = list(alpha = NULL,
                                 lambda = NULL,
                                 trend_decay = NULL,
                                 time_weight = NULL))

# Models --------------

list_models <- c("naive","snaive","croston","ets","theta",
                 "arima","tbats","nn","prophet")

# Call function --------------

# Get best model with algo study with cleansing (Default)

obj1 <- autoforecast(data, models = list_models, algo_study = TRUE)

# Get best model with algo study without cleansing

obj1 <- autoforecast(data, models = list_models, algo_study = TRUE, clean_series = FALSE)

# Get best 3 models with algo study

obj3 <- autoforecast(data, models = list_models, output_models = c(1,2,3), algo_study = TRUE)

# Just forecast with all models

obj4 <- autoforecast(data, models = list_models, algo_study = FALSE)

# Get 3 best models working with a list

data2 <- list(y = as.numeric(AirPassengers), start_date = "2010-01-01")

obj5 <- autoforecast(data2, models = list_models, output_models = c(1,2,3))

#---
