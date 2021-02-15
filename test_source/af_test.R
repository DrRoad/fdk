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

sales <- readRDS("~/Dropbox/Sanofi/data/cs.rds") %>% 
  mutate(date = as.Date(date))
cf <- readRDS("~/Dropbox/Sanofi/data/cf.rds")

new_data <- sales %>% 
  left_join(cf, by = c("forecast_item", "date"))

.data_init <- new_data
y_var <- "sales"
date_var <- "date"
key = "forecast_item"
reg_name = "regressor"
reg_value = "quantity"
date_format = "ymd"
freq = 12


d1 <- prescribe_ts(.data = new_data
             , key = "forecast_item"
             , y_var = "sales"
             , date_var = "date"
             , reg_name = "regressor"
             , reg_value = "quantity"
             , freq = 12
             , date_format = "ymd")

source("R/data_cleansing.R")
source("R/data_preparation.R")
source("R/feature_engineering.R")
source("R/data_validation.R")

# Parameter ---------------------------------------------------------------

parameter <- list(gam = list(smoothed_features = list(trend = list(k = NA, bs = "tp"))
                             , formula = NULL
                             , excluded_features = NULL
                             , time_weight = 1
                             , link_function = "gaussian")
                  , glmnet = list(alpha = .9, lambda = NULL
                                  , time_weight = 1
                                  , trend_decay = .9
                                  , excluded_features = list("quarter_seas")
                                  , formula = NULL))

optim_conf <- list(test_size = 6, lag_ref = 3, export_fit = FALSE)


.data <- d1 %>%
  filter(key == "AE: 424199") %>% 
  pull(data) %>% 
  .[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts(hierarchy_seas = T, lag_var = list(y_var = 3)
                         , numeric_seas = F) %>% 
  clean_ts(winsorize_config = list(apply_winsorize = FALSE)
           , imputation_config = list(impute_method = "none"
                                      , na_regressor = TRUE
                                      , na_missing_dates = TRUE))



%>% 
  fit_ts(ts_model = "gam", parameter = parameter, optim = optim) %>% 
  summary_ts()


  optim_ts(.data = ., test_size = 6, lag_ref = 3, ts_model = "gam"
           , parameter = parameter, export_fit = FALSE)


d2 <- d1 %>% 
  mutate(n = map_int(data, ~nrow(.x))) %>% 
  filter(str_detect(key, "^DK|^SE"))

registerDoParallel(cores = 2)

l <- foreach(i = d2$key[1:5]) %dopar% {
  d1 <- prescribe_ts(.data = new_data
                     , key = "forecast_item"
                     , y_var = "sales"
                     , date_var = "date"
                     , reg_name = "regressor"
                     , reg_value = "quantity"
                     , freq = 12
                     , date_format = "ymd")
  .data <- d2 %>% 
    filter(key == i) %>% 
    pull(data) %>% 
    .[[1]] %>% 
    validate_ts()%>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = T)
             , imputation_config = list(impute_method = "kalman"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE)) %>%
    optim_ts(.test_size = 6, .lag_ref = 3, .data = ., ts_model = "gam", parameter = parameter)
  .data
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


