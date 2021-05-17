
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


# Pipeline ----------------------------------------------------------------

pipeline_ts(data_presc[1:3,], .pipeline_conf = get_default_pipeline_conf())
