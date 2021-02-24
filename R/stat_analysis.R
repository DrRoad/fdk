


# GAM ####


oc_data <- get_oc_data(db = list("full_sales", "full_forecast", "regressor", "forecast_item_info")
            , countries = c("NO", "SE", "DK", "FI", "NL", "BE", "IN", "LV", "LT", "EE")
            , gbus = "GEM"
            , date_cycle = "2021-02-01")

input_data <- oc_data$full_sales %>%
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
  mutate(series_type = factor(series_type, c("history", "forecast"))) %>% 
  group_nest(forecast_item)



.data <- input_data$data[[1]]

imp_kalman <- function(.data){
  
  if(all(is.na(.data$reg_name))){
    .data %>% 
      dplyr::select(date, sales, series_type) %>% 
      distinct()
  } else {
    tryCatch(
      {
        kalman_int <- function(vec){
          ts1 <- ts(vec, frequency = 12)
          as.numeric(round(imputeTS::na_kalman(x = ts1), 0))
        }
        
        fcst <- .data %>% 
          filter(series_type %in% c(0, "forecast")) %>% 
          dplyr::select(date, sales, series_type) %>% 
          distinct()
        
        sales_new <- .data %>% 
          filter(series_type %in% c(1, "history")) %>%
          group_by(date) %>% 
          summarise(sales = mean(sales)
                    , reg_value = any(reg_value != 0)
                    , .groups = "drop") %>% 
          mutate(sales = case_when(
            reg_value == T ~ NA_real_
            , TRUE ~ sales)) %>% 
          dplyr::select(-reg_value)
        
        sales_clean <- sales_new %>% 
          pull(sales) %>%
          kalman_int()
        
        
        sales_new %>% 
          mutate(sales = sales_clean
                 , series_type = "history") %>% 
          bind_rows(fcst)
      }
      , error = function(err) {
        .data %>% 
          dplyr::select(date, sales, series_type) %>% 
          distinct()
      }
    )
  }
}


input_data2 <- input_data %>% 
  #slice(1:10) %>% 
  mutate(data = map(data, ~imp_kalman(.x)))



active_items <- fii %>% 
  filter(is_excluded == F) %>% 
  dplyr::select(forecast_item, lifecycle, is_excluded)

platinum <- fii %>% 
  filter(p2w_strategy == "Platinum", is_excluded == F) %>% 
  pull(forecast_item)

forecastable <- fii %>% 
  filter(is_excluded == F, forecastability == T
         #, str_detect(lifecycle, "Switch-Out|EOL", negate = T)
  ) %>% 
  pull(forecast_item)



# Analysis ----------------------------------------------------------------

d1$data[[4]] %>% 
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  clean_ts(winsorize_config = list(apply_winsorize = T
                                   , add_transformations = T)
           , imputation_config = list(hierarchical_seas = T))


