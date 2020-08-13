
# autoforecast - testing

# If needed: load_pkgs --------------

require(pacman)
pacman::p_load(forecast,tidyverse,seastests,tsfeatures,dplyr,
               lubridate,zoo,DescTools,dvmisc,ggplot2,tsibble,
               prophet,imputeTS,glmnet, tictoc, fastDummies,
               devtools,git2r, foreach, doSNOW, snow)

# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)
# pkg <- "https://emea-aws-gitlab.sanofi.com:3001/statistical_forecasting/packages/autoforecast.git"
# cred <- git2r::cred_user_pass(rstudioapi::askForPassword("Username"), rstudioapi::askForPassword("Password"))
# devtools::install_git(pkg, credentials = cred)

# Skus --------------

Skus <- c("SE: 198026", "DK: 578280", "DK: 688222")
# Skus <- Skus[1]

# Reading Csv file

data0 <- read.csv("../demo_data.csv", sep=",")

# Formatting
data0$key <- as.character(data0$key)
data0$y <- as.numeric(data0$y)
data0$date <- as.Date(data0$date,format="%m/%d/%Y")
data0$reg <- as.numeric(data0$reg)

# Filtering selected sku and dates

data0 <- data0 %>% filter(key %in% Skus)
data <- data0 %>% filter(date < "2020-07-01") %>% arrange(key,date)
head(data)

# Parameters --------------

parameters <- list(params_arima = NULL, params_croston = NULL,
                   glmnet = list(alpha = NULL,
                                 lambda = NULL,
                                 trend_decay = NULL,
                                 time_weight = NULL))

# Models --------------

list_models <- c("naive","snaive","croston","ets","theta",
                 "arima","tbats","ensemble","stlm","theta_dyn",
                 "nn","prophet","tslm")

# Parameters

frequency <- 52

# Algo study

algos1 <- data %>% build_ts(frequency = frequency) %>% algo_study(models=list_models)

# Generate forecast

fcst1 <- data %>% build_ts(frequency = frequency) %>% gen_fcst(models=list_models,h=36)

# Autoforecast

obj1 <- autoforecast(data, frequency = frequency, models = list_models, algo_study = TRUE)

#---
