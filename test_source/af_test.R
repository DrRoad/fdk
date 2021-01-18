library(tsibble)
library(tidyverse)
library(parallel)
library(doSNOW)
library(doParallel)
library(furrr)


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

d1 <- prescribe_ts(.data = new_data
             , key = "forecast_item"
             , y_var = "sales"
             , date_var = "date"
             , reg_name = "regressor"
             , reg_value = "quantity"
             , freq = 12
             , date_format = "ymd")


.data <- d1 %>%
  filter(key == "BE: 106384") %>% 
  pull(data) %>% 
  .[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts(lag_var = list(y_var = c(1, 3)))




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


