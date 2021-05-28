
# Package -----------------------------------------------------------------

library(fdk)
library(tidyverse)
library(parallel)
library(doParallel)
library(forecast)
library(ggrepel)

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
  bind_rows(co_data) %>% 
  filter(date<"2021-05-01")


data.table::fwrite(sales_out, file = "co_sales_out_reg.csv")

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

.fit <- co_presc$data[[1]] %>% 
  validate_ts() %>% 
  feature_engineering_ts() %>% 
  clean_ts() %>% 
  optim_ts(.data = .
           , ts_model = c("glmnet", "gam", "glm", "arima", "ets", "croston")
           , optim_conf = get_default_optim_conf()
           , parameter = get_default_hyperpar()
           , export_fit = T)

o2 <- .fit %>% 
  forecast_ts(horizon = 10)

.fit %>% 
  plot_ts()


out$forecast[[1]] %>% 
  ggplot(aes(date_var, forecast))+
  geom_line()

out %>% 
  dplyr::select(index, forecast) %>% 
  #slice(1, 70) %>% 
  unnest(forecast) %>% 
  ggplot(aes(date_var, forecast, col = as.factor(index)))+
  geom_line()

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
