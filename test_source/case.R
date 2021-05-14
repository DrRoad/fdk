
# Importing data ----------------------------------------------------------

source_conf = list(source = "oc"
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

data_init <- import_data(source_conf = source_conf)


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


# Example -----------------------------------------------------------------


recipe_1 <- function(.data){
  .data %>% 
  validate_ts() %>% 
    feature_engineering_ts() %>% 
    optim_ts(ts_model = c("arima", "glmnet", "gam", "glm", "ets")
             , optim_conf = get_default_optim_conf()
             , parameter = get_default_hyperpar()
             , export_fit = T)
}

optim_1 <- recipe_1(data_presc$data[[1]])

# Multiple ----------------------------------------------------------------


  
