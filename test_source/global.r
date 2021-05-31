
# GLOBAL ####

source_conf = list(source = "oc"
                 , date_cycle = "2021-05-01"
                 , db = list("full_sales"
                             , "full_forecast"
                             , "regressor"
                             #, "forecast_item_info"
                             )
                 , countries = c("NO", "SE", "DK", "FI", "NL", "BE", "IN", "LV", "LT", "EE")
                 #, countries = c1
                 , filters = list(category = "HistoricalSales"
                                  , cycle_category = "before_cleansing")
                 , gbus = c("GEM", "CHC", "SPC")
                 , join_hist_forecast = T)

parameter <- get_default_hyperpar()
oc_data <- import_data(source_conf = source_conf)
#oc_data_2 <- import_data(source_conf = source_conf)
forecast_item_list <- oc_data$sales$forecast_item %>% 
  unique() %>% 
  sort()

insight_data <- get_insight_data(oc_data = oc_data
                                 , key = "NO: 399582"
                                 , parameter = get_default_hyperpar())

get_graph_stat(insight_data = insight_data, graph_type = "seas_me")





 get_tables(insight_data = insight_data, table_type = "feature_imp")
 
 
 oc_data_2$sales %>% data.table::fwrite(file = "C:/Users/I0415596/Desktop/oc_data_210519.csv")
 
 