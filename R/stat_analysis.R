
# GAM ####


source_oc = list(source = "oc"
            , date_cycle = "2021-03-01"
            , db = c("full_sales", "full_forecast", "regressor", "forecast_item_info")
            , countries = c("NO", "SE", "DK", "FI", "NL", "BE", "IN", "LV", "LT", "EE")
            , gbus = "GEM"
            , do_prescribe = T)

oc_data <- import_data(source_conf = source_oc)

# Functions ---------------------------------------------------------------

get_seas_me <- function(.data, min_date = NULL
                        , max_date = NULL, robust_lm = F
                        , ci_me = .95){
  
  z_ce <- tibble::tribble(
    ~ci,   ~z,
    0.80, 1.282,
    0.85,  1.44,
    0.90, 1.645,
    0.95,  1.96,
    0.99, 2.576,) %>% 
    filter(ci == ci_me) %>% 
    pull(z)
  
  if(robust_lm == T){
    library(MASS)
    lm_reg <- match.fun("rlm")
  } else {
    lm_reg <- match.fun("glm")
  }
  
  if(is.null(min_date)==T){
    min_date <- min(.data[["date_var"]])
  }
  
  if(is.null(max_date)==T){
    max_date <- max(.data[["date_var"]])
  }
  
  .data %>% 
    dplyr::filter(date_var >= min_date
                  , date_var <= max_date) %>% 
    dplyr::select(-matches("date_var|y_var|trend"), y_var_d1) %>% 
    lm_reg(y_var_d1~., data = .) %>% 
    ggeffects::ggeffect(terms = c("month_seas")) %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(predicted = round(predicted, 2)
           , conf_low = predicted - z_ce * std_error
           , conf_high = predicted + z_ce * std_error
           , month_seas = factor(x, levels = month.abb)) %>% 
    dplyr::select(month_seas, everything(), -x, -group)
}


join_oc_data <- function(.oc_data, .key){
  forecast_data <- .oc_data$knx_forecast %>% 
    filter(key == .key) %>% 
    pull(data) %>% 
    .[[1]]
  
  sales_data <- .oc_data$knx_sales %>% 
    filter(key == .key)%>% 
    pull(data) %>% 
    .[[1]]
  
  forecast_data %>% 
    dplyr::select(date_var, y_var) %>% 
    distinct() %>% 
    mutate(reg_name = "is_history", reg_value = 0)%>% 
    bind_rows(forecast_data, .) %>% 
    bind_rows(
      sales_data %>% 
        dplyr::select(date_var, y_var) %>% 
        distinct() %>% 
        mutate(reg_name = "is_history", reg_value = 1) %>% 
        bind_rows(sales_data, .)
      , .
    ) %>%
    validate_ts() %>% 
    feature_engineering_ts() %>% 
    clean_ts(winsorize_config = list(apply_winsorize = T, add_transformations = T)
             , imputation_config = list(impute_method = "kalman"
                                        , na_regressor = TRUE
                                        , na_missing_dates = TRUE
                                        , add_transformations = T)) %>% 
    mutate(is_history = factor(is_history, levels = c(1, 0))) %>% 
    group_by(is_history) %>% 
    mutate(y_var_d1 = tsibble::difference(y_var, 1), .after = "y_var") %>% 
    slice(-1) %>% 
    ungroup()
}


seas_me_data <- join_oc_data(.oc_data = oc_data, .key = key) %>% 
  group_nest(is_history) %>% 
  mutate(data = map(data, ~get_seas_me(.x, robust_lm = F))) %>% 
  unnest(data)

graph_seas_me(seas_me_data)


data_in <- oc_data$knx_sales %>%
  filter(key =="BE: 115110") %>% 
  pull(data) %>%
  .[[1]] %>%
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  distinct()

gam_fit <- data_in %>% 
  fit_ts(.data = ., ts_model = "gam", parameter = parameter)

gam_fitted <- predict(gam_fit, newdata = data_in, se.fit = T) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(lwr = fit - 1.96*se.fit
         , upr = fit + 1.96*se.fit) %>% 
  ungroup() %>% 
  janitor::clean_names()

data_in %>% 
  bind_cols(gam_fitted) %>% 
  ggplot()+
  geom_ribbon(aes(date_var, ymin = lwr, ymax= upr)
              , fill = "blue", alpha = .3)+
  geom_line(aes(date_var, y_var))+
  geom_line(aes(date_var, fit), col = "red")
  
gam_fitted <- predict_ts(fit = gam_fit, ts_model = "gam"
           , optim_conf = optim_conf
           , parameter = parameter
           , add_fitted = T, .data = data_in)

gam_derivative <- gam_fit %>% 
  gratia::derivatives() %>% 
  as_tibble() %>% 
  rename(trend = data) %>% 
  ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(aes(trend, ymin = lower, ymax = upper), alpha = .2, fill = "red")+
  geom_line(aes(trend, derivative))+
  labs(x = "Time index (continuous)", y = "Trend derivative")+
  theme_minimal()

plotly::ggplotly(gam_derivative)



graph_seas_me <- function(seas_me_data, has_forecast = F){
  g1 <- seas_me_data %>% 
    ggplot()+
    geom_hline(yintercept = 0, linetype = "dotted", col = "red", size = 1)+
    geom_linerange(aes(x = month_seas, ymin = conf_low, ymax = conf_high
                       , col = is_history)
                   , position = position_dodge(width = .3)
                   #, col="black"
                   )+
    geom_point(aes(month_seas, predicted
                   , col = is_history), position = position_dodge(width = .3)
               , size = 3)+
    theme_minimal()+
    theme(axis.text = element_text(colour = "black", size = 12)
          , axis.title = element_text(colour = "black", size = 15))+
    labs(x = "", y = "Marginal Seasonal Effects")
  
    plotly::ggplotly(g1)
}




%>% 
  #rename(is_history = group) %>% 
  
  







  

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


