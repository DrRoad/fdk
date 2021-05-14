
# GLOBAL ####

source_conf = list(source = "oc"
                 , date_cycle = "2021-05-01"
                 , db = list("full_sales"
                             , "full_forecast"
                             , "regressor"
                             #, "forecast_item_info"
                             )
                 , countries = c("NO", "SE", "DK", "FI", "NL", "BE", "IN", "LV", "LT", "EE")
                 #, countries = "RU"
                 , filters = list(category = "HistoricalSales"
                                  , cycle_category = "before_cleansing")
                 , gbus = c("GEM")
                 , join_hist_forecast = T)

parameter <- get_default_hyperpar()
oc_data <- import_data(source_conf = source_conf)
forecast_item_list <- oc_data$sales$forecast_item %>% 
  unique() %>% 
  sort()

insight_data <- get_insight_data(oc_data = oc_data
                                 , key = "FI: 474452"
                                 , parameter = get_default_hyperpar())

get_graph_stat(insight_data = insight_data, graph_type = "seas_me")





 get_tables(insight_data = insight_data, table_type = "year_agg")
 