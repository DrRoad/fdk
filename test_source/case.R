
# Package -----------------------------------------------------------------

library(fdk)
library(tidyverse)
library(parallel)
library(doParallel)
library(forecast)
library(ggrepel)

# Importing data ----------------------------------------------------------

source_hist = list(source = "oc"
                   , date_cycle = "2021-05-01"
                   , db = list("full_sales"
                               #, "full_forecast"
                               , "regressor"
                               #, "forecast_item_info"
                   )
                   , countries = c("NO", "SE", "DK", "FI", "NL", "BE", "IN", "LV", "LT", "EE")
                   #, countries = "RU"
                   , filters = list(category = "HistoricalSales"
                                    , cycle_category = "before_cleansing")
                   , gbus = c("GEM")
                   , join_hist_forecast = T)

data_init <- import_data(source_conf = source_hist)

# Prescribe data ----------------------------------------------------------

data_presc <- data_init$sales %>% 
  prescribe_ts(.data_init =.
               , key = "forecast_item"
               , y_var = "sales"
               , date_var = "date"
               , reg_name = "reg_name"
               , reg_value = "reg_value"
               , freq = 12
               , date_format = "ymd")


# UNique ------------------------------------------------------------------


profvis::profvis(
  {
    p <- data_presc$data[[1]] %>% 
      validate_ts() %>% 
      feature_engineering_ts(ma_var = list(y_var  = c(2, 0))) %>% 
      clean_ts() %>% 
      optim_ts(.data = .
               , ts_model = c("arima")
               , optim_conf = get_default_optim_conf()
               , parameter = get_default_hyperpar()
               , export_fit = F)
  }
)

# Pipeline ----------------------------------------------------------------

pipeline_conf <- list(
  prescribe = list(.data_init = data_init$sales
                   , key = "forecast_item"
                   , y_var = "sales"
                   , date_var = "date"
                   , reg_name = "reg_name"
                   , reg_value = "reg_value"
                   , freq = 12
                   , date_format = "ymd")
  , validate = list(na_values = list(y_var = 0, reg_value = 0, reg_name = ""))
  , feature_engineering = list(lag_var = list()
                               , ma_var = list()
                               , numeric_seas = FALSE
                               , hierarchy_seas = FALSE
  )
  , optim = list(
    ts_model = c("arima",
                 "glmnet"
                 , "gam"
                 , "glm"
                 , "ets"
                 )
    , optim_conf = get_default_optim_conf()
    , parameter = get_default_hyperpar()
    , export_fit = T
    )
  )

optim_results <- pipeline_ts(data_presc[1:1,], .pipeline_conf = pipeline_conf)


optim_results %>% 
  plot_ts()

