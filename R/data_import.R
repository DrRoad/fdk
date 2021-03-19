

#' Title
#'
#' @param source_conf 
#'
#' @return
#' @export
#'
#' @examples
import_data <- function(source_conf = source_oc){
  if(source_conf$source == "oc"){
    oc_data <- get_oc_data(db = source_conf$db
                           , countries = source_conf$countries
                           , gbus = source_conf$gbus
                           , date_cycle = source_conf$date_cycle)
    
    oc_data <- oc_data$full_sales %>%
      filter(category == "HistoricalSales"
             , cycle_category == "after_cleansing") %>% 
      dplyr::select(forecast_item, date, sales = m_sales) %>% 
      mutate(series_type = "history") %>% 
      bind_rows(
        oc_data$full_forecast %>% 
          filter(cycle_category == "after_cleansing") %>% 
          dplyr::select(forecast_item, date, sales = forecast)
      ) %>% 
      left_join(oc_data$regressor, by = c("forecast_item", "date"="reg_date")) %>%
      replace_na(replace = list(series_type = "forecast", reg_value = 0)) %>% 
      mutate(series_type = factor(series_type, c("history", "forecast")))
    
    knx_forecast <- input_data %>% 
      filter(series_type == "forecast") %>% 
      dplyr::select(-series_type)
    
    knx_sales <- input_data %>% 
      filter(series_type == "history") %>% 
      dplyr::select(-series_type)
    
    if(source_conf$do_prescribe == T){
      knx_forecast <- knx_forecast %>% 
        prescribe_ts(.data = .
                     , key = "forecast_item"
                     , y_var = "sales"
                     , date_var = "date"
                     , reg_name = "reg_name"
                     , reg_value = "reg_value"
                     , freq = 12
                     , date_format = "ymd")
      
      knx_sales <- knx_sales %>% 
        prescribe_ts(.data = .
                     , key = "forecast_item"
                     , y_var = "sales"
                     , date_var = "date"
                     , reg_name = "reg_name"
                     , reg_value = "reg_value"
                     , freq = 12
                     , date_format = "ymd")
    }
    list(knx_sales = knx_sales, knx_forecast = knx_forecast)
  }
}