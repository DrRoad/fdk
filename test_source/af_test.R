library(tsibble)
library(tidyverse)
library(parallel)
library(doSNOW)
library(doParallel)
library(furrr)
library(profvis)
library(imputeTS)
library(sparklyr)
library(mgcv)
library(glmnet)
library(forecast)
library(plotly)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)

sales_tmp %>% 
  mutate(reg_value = )


# Initial -----------------------------------------------------------------

new_data <- sales %>% 
  left_join(cf, by = c("forecast_item", "date"))

source("R/data_cleansing.R")
source("R/data_preparation.R")
source("R/feature_engineering.R")
source("R/data_validation.R")
source("R/auxiliar.R")
source("R/fit_gam.R")
source("R/fit_glmnet.R")
source("R/fit_glm.R")
source("R/fit_arima.R")
source("R/fit_croston.R")
source("R/fit_ets.R")
source("R/model_optimization.R")
source("R/predict_ts.R")
source("R/data_import.R")
source("R/stat_analysis.R")

presc_data <- prescribe_ts(.data_init = new_data
             , key = "forecast_item"
             , y_var = "sales"
             , date_var = "date"
             , reg_name = "regressor"
             , reg_value = "quantity"
             , freq = 12
             , date_format = "ymd")


microbenchmark::microbenchmark(
results <- pd1$data[[2]] %>% 
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  clean_ts() %>% 
  optim_ts(.data = .
           , ts_model = c("arima", "gam", "glm", "ets", "glmnet")
           , optim_conf = get_default_optim_conf()
           , parameter = parameter
           , export_fit = F)
)

# Parameter ---------------------------------------------------------------

optim_conf <- list(test_size = 6, lag = 1)


tdata <- pd1$data[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  clean_ts(winsorize_config = list(apply_winsorize = TRUE)
           , imputation_config = list(impute_method = "none"
                                      , na_regressor = FALSE
                                      , na_missing_dates = FALSE))




my_mkt <- presc_data %>% 
  filter(str_detect(key, "IN|DK|SE|NO|FI|NL|BE|EE|LT|LV")) %>% 
  pull(key)


# TESTING

d1 <-  optim_ts(.data = tdata
         , ts_model = c("arima", "glmnet", "gam", "glm", "ets")
         , optim_conf = get_default_optim_conf()
         , parameter = get_default_hyperpar()
         , export_fit = F)



profvis(
optim_ts(.data = .data
         , ts_model = c("glmnet")
         , optim_conf = get_optim_conf()
         , parameter = get_hyperpar()
         , export_fit = F)
)


# -------

.data %>% 
  mutate(distance = 0.3*mape + 0.7*spa_d, .after = 1) %>% 
  arrange(distance)

results %>% 
  mutate(spa_d = 1 - spa
         , score = 0.5*mape + 0.5*abs(spa_d), .after = 1) %>% 
  # mutate(spa_d_log = log(spa_d + 1)
  #        , mape_log = log(mape + 1)) %>% 
  ggplot()+
  geom_density2d_filled(aes(mape, spa_d), alpha = .9)+
  geom_hline(yintercept = 0, col = "white")+
  geom_point(aes(mape, spa_d
                 , shape = model
                 , col = model
                 , size = 100*(score^-1)), fill = NA
             #, col = "white"
             #, shape = 21
             )+
  scale_x_log10()+
  #scale_y_log10()+
  theme_minimal()

plotly::ggplotly(p)


count <- 0
glmnet <- map(.x = my_mkt[1:10], .f = function(x){
  count <<- count + length(x)
  d1 %>%
    filter(key == x) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = T)
             , imputation_config = list(impute_method = "none"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>%
    optim_ts(.data = ., ts_model = "glmnet", optim_conf = optim_conf
             , parameter = parameter, export_fit = F)
})
  
count <- 0
gam <- map(.x = my_mkt, .f = function(x){
  count <<- count + length(x)
  d1 %>%
    filter(key == x) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = F)
             , imputation_config = list(impute_method = "none"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>%
    optim_ts(.data = ., ts_model = "gam", optim_conf = optim_conf
             , parameter = parameter, export_fit = F)
})

count <- 0
glm <- map(.x = my_mkt, .f = function(x){
  count <<- count + length(x)
  d1 %>%
    filter(key == x) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = F)
             , imputation_config = list(impute_method = "none"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>%
    optim_ts(.data = ., ts_model = "glm", optim_conf = optim_conf
             , parameter = parameter, export_fit = F)
})

#count_i <- 0
arima <- map(.x = my_mkt, ~{
  #count_i <<- count_i + length(x)
  d1 %>%
    filter(key == .x) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = F)
             , imputation_config = list(impute_method = "none"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>%
    optim_ts(.data = ., ts_model = "arima", optim_conf = optim_conf
            , parameter = parameter, export_fit = F)
})

count_i = 0
ets <- map(.x = my_mkt[6], ~{
  count_i <<- count_i + length(.x)
  d1 %>%
    filter(key == .x) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = F)
             , imputation_config = list(impute_method = "none"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>%
    optim_ts(.data = ., ts_model = "ets", optim_conf = optim_conf
             , parameter = parameter, export_fit = F)
})




opt_sum <- function(.key){
  d1 %>%
    filter(key == .key) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = T)
             , imputation_config = list(impute_method = "none"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>%
    optim_ts(.data = ., ts_model = "glmnet"
             , optim_conf = optim_conf
             , parameter = parameter, export_fit = F) %>% 
    summary_ts()
}



.data <- d1 %>%
  filter(key == "AE: 424283") %>% 
  pull(data) %>% 
  .[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  clean_ts(winsorize_config = list(apply_winsorize = F)
           , imputation_config = list(impute_method = "none"
                                      , na_regressor = TRUE
                                      , na_missing_dates = TRUE))

.data <- d1 %>%
  filter(key == "AE: 424283") %>% 
  pull(data) %>% 
  .[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  clean_ts(winsorize_config = list(apply_winsorize = FALSE)
           , imputation_config = list(impute_method = "none"
                                      , na_regressor = TRUE
                                      , na_missing_dates = TRUE)) %>% 
  optim_ts(.data = ., ts_model = "glm", optim_conf = optim_conf
           , parameter = parameter, export_fit = T) %>% 
  summary_ts()

f <- d1 %>%
  filter(key == "AE: 424283") %>% 
  pull(data) %>% 
  .[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts(hierarchy_seas = T, numeric_seas = T) %>% 
  clean_ts(winsorize_config = list(apply_winsorize = T
                                   , add_transformations = T)
           , imputation_config = list(impute_method = "kalman"
                                      , na_regressor = TRUE
                                      , na_missing_dates = TRUE
                                      , add_transformations = T
                                      , na_value = 0)) %>% 
  pivot_longer(cols = matches("y_var")) %>% 
  ggplot()+
  geom_line(aes(date_var, value, col = name)) 


s1 <- d1 %>%
  filter(key == "DK: 421908") %>% 
  pull(data) %>% 
  .[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts(hierarchy_seas = T) %>% 
  clean_ts(winsorize_config = list(apply_winsorize = T
                                   , add_transformations = T)
           , imputation_config = list(impute_method = "none"
                                      , na_regressor = TRUE
                                      , na_missing_dates = TRUE
                                      , add_transformations = T
                                      , na_value = NULL)) %>% 
  mutate(y_var_d1 = tsibble::difference(y_var)) %>% 
  slice(1:(n())) %>% 
  na.omit()

fit <- glm(y_var_d1 ~ month_seas, data = s1)
ggeffects::ggemmeans(fit, terms = "month_seas", ci.lvl = .8) %>% 
  plot()








%>% 
  fit_ts(ts_model = "gam", parameter = parameter, optim = optim) %>% 
  summary_ts()


  optim_ts(.data = ., test_size = 6, lag_ref = 3, ts_model = "gam"
           , parameter = parameter, export_fit = FALSE)


d2 <- d1 %>% 
  mutate(n = map_int(data, ~nrow(.x))) %>% 
  filter(str_detect(key, "^DK|^SE"))

registerDoParallel(cores = 5)

l <- foreach(i = d1$key[4], .packages = c("tidyverse", "mgcv", "glmnet", "stlplus")) %dopar% {
  d1 %>%
    filter(key == i) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = FALSE)
             , imputation_config = list(impute_method = "none"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>% 
    optim_ts(.data = ., ts_model = "gam", optim_conf = optim_conf
             , parameter = parameter, export_fit = FALSE) %>% 
    summary_ts()
}





names(l) <- d2$key[1:5]
l2 <- l %>% 
  enframe() %>% 
  mutate(mape = map_dbl(value, ~.x[["mape"]]))


l <- .data$.data

.data$.data %>% 
  ggplot()+
  geom_line(aes(date_var, y_var))+
  geom_line(aes(date_var, y_var_clean), col ="red")+
  geom_line(aes(date_var, y_var_imp), col = "purple")+
  geom_line(aes(date_var, y_var_winso_imp), col ="blue")



profvis::profvis(
  .data %>% 
    validate_ts()
)



cl <- makeCluster(3, type = "SOCK")
doSNOW::registerDoSNOW(cl = cl)



mb <- microbenchmark::microbenchmark(
  seq = lapply(d1$data[1:100], FUN = function(x) validate_ts(x))
  , multi = foreach(i = seq_along(d1$key[1:100]), .packages = "tidyverse") %dopar% {
  validate_ts(d1$data[[i]])
  }
  , future = future_map(d1$data[1:100], ~validate_ts(.x))
  , mc = mclapply(d1$data[1:100], FUN = function(x) validate_ts(x)
                  , mc.cores = 3)
  , times = 20)


profvis::profvis(
  k <- mclapply(d1$data[1:100], FUN = function(x) validate_ts(x)
                , mc.cores = 3)
)



logs <- bind_rows(af_log, map_df(k, ~.x[[2]]))

l1 <- logs %>% 
  unnest(log) %>% 
  filter(has_reg == T)




k <- foreach(i = seq_along(d1$key[1:100])
             , .packages = c("tidyverse")) %dopar% {
               job_1(d1$data[[i]])
}

job_1 <- function(.data){
  .d1 <- validate_ts(.data)
  new_log <- logger
  list(.d1, new_log)
}


