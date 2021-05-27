
# Package -----------------------------------------------------------------


# Data --------------------------------------------------------------------

source_conf = list(source = "oc"
                   , date_cycle = "2021-06-01"
                   , db = list("full_sales"
                               #, "full_forecast"
                               , "regressor"
                               #, "forecast_item_info"
                   )
                   , countries = "CO"
                   , filters = list(category = "HistoricalSales"
                                    , cycle_category = "before_cleansing")
                   , gbus = c("GEM", "CHC", "SPC")
                   , join_hist_forecast = T)


co_data <- import_data(source_conf = source_conf)[["sales"]]

co_data_tmp <- co_data %>% 
  dplyr::select(forecast_item, date, sales) %>% 
  distinct()

## Regressors

sales_out_0 <- rdata2obj("//E21flsbcnschub/BCN_SC_HUB/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/Outputs/DATABASE/Jun - 2021/external_regressors.RData")

sales_out <- sales_out_0 %>% 
  ungroup() %>% 
  dplyr::select(-any_of(c("loc", "gmid", "gbu"))) %>% 
  left_join(co_data_tmp, by = c("forecast_item", "date")) %>% 
  dplyr::select(names(co_data)) %>% 
  bind_rows(co_data)

# Prescribe ---------------------------------------------------------------

co_presc <- sales_out %>% 
  prescribe_ts(.data_init =.
               , key = "forecast_item"
               , y_var = "sales"
               , date_var = "date"
               , reg_name = "reg_name"
               , reg_value = "reg_value"
               , freq = 12
               , date_format = "ymd")

o1 <- co_presc$data[[4]] %>% 
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  clean_ts() %>% 
  optim_ts(.data = .
           , ts_model = c("glmnet")
           , optim_conf = get_default_optim_conf()
           , parameter = get_default_hyperpar()
           , export_fit = T)

o2 <- o1 %>% 
  mutate(coef = map_lgl(fit, ~broom::tidy(.x)[["term"]] %>% 
                      str_detect("so_regre") %>% 
                      any()))

o2 %>% 
  lm(mape ~ as.factor(coef), data = .) %>% 
  broom::tidy()



o2 %>% 
  ggplot(aes(coef, rank_agg)) +
  geom_boxplot()+
  labs(x = "is SO included?")
