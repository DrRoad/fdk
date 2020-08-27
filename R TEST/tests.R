
# Package ------------------------------------------------------------------

require(tidyverse)
require(glmnet)
require(stlplus)
require(forecast)


# Import ------------------------------------------------------------------

demo_data <- readRDS("demo_data_update.rds")

demo_1 <- demo_data %>% 
  dplyr::filter(forecast_item == "FI: 592905") %>% 
  pivot_wider(names_from = reg_name, values_from = reg_value) %>% 
  mutate_at(.vars = vars(4:last_col()), ~ifelse(is.na(.x), 0, .x)) %>% 
  janitor::clean_names() %>% 
  select(-x0)

# Cleansing ---------------------------------------------------------------

demo_2 <- demo_1 %>% 
  bind_rows(
    tibble(volume = 0
           , date = seq.Date(to = (min(.$date)-months(1))
                             , from = (min(.$date) - months(3))
                             , by = "months")
           , .rows = 3)
  ) %>%
  fill_(names(.), .direction = "down") %>% 
  arrange(date)


source("R/get_snaive.R")
source("R/get_forecast.R")
source("R/get_ets.R")
source("R/get_arima.R")
source("R/get_glm.R")
source("R/get_glmnet.R")
source("R/get_croston.R")
source("R/get_nn.R")
source("R/get_tbats.R")
source("R/cleansing.R")
source("R/auxiliar.R")

# Fit ---------------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.7, to = 1, by = 0.05)
                    , trend_discount = seq(from = 0.9, to = 1, by = 0.05)
                    , alpha = seq(from = 0, to = 1, by = 0.10))
grid_glm <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.02)
                           , trend_discount = seq(from = 0.8, to = 1, by = 0.02))

parameter <- list(glmnet = list(time_weight = .94, trend_discount = .93, alpha = 0, lambda = .1
                                , grid_glmnet = grid_glmnet
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.1
                                             , n_best_model = 1))
                  , croston = list(alpha = 0.1)
                  , glm = list(time_weight = .99, trend_discount = 0.97
                               , grid_glm = grid_glm
                               , job = list(x_excluded = NULL
                                            , random_search_size = 0.1
                                            , n_best_model = 1))
                  , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                  , ets = list(ets = "ZZZ"))

.data <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts() %>% 
  get_glmnet(parameter = parameter) %>%
  get_forecast(horizon = 100)
  

map(sample(1:100, 4), ~update_parameter(old_parameter = parameter, new_parameter = parameter$glm$grid[.x, ], model = "glm"))









tictoc::tic()
map(.x = c("arima", "glmnet")
    , .f = ~optim_ts(.data, test_size = 6, lag = 3, parameter = parameter, model = .x)) %>% 
  bind_rows()
tictoc::toc()


.data <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts() %>% 
  get_neural_network() %>% 
  get_forecast(horizon = 10)

foreach(c)


splits <- split_ts(.data, test_size = 6, lag = 3)
split_1 <- splits[[1]]





parameter <- update_parameter(parameter = parameter, new = grid[1,])


tictoc::tic()
optim_ts(.data, test_size = 4, lag = 4, parameter = parameter
         , model = c("glmnet", "glm", "arima", "tbats", "neural_network"
                     , "seasonal_naive", "croston", "ets"), parallel = T)
tictoc::toc()





autoforecast <- function(){
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
    clean_ts() %>% 
  c("glmnet") %>% 
    map(~optim_ts(.data, test_size = 6, lag = 1, parameter = parameter, model = .x))
}

.fit_output <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts(method = "winsorize") %>% 
  get_arima_experimental(parameter = parameter) %>% 
  get_forecast_experimental(horizon = 30)

profvis::profvis({
optim_ts(.data, test_size = 6, lag = 1, parameter = parameter, model = "arima")
})


tictoc::tic()
  foreach(model = c("arima", "glmnet"), .combine = "rbind") %dopar% {
    .data <- demo_2 %>% 
      prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
      clean_ts()
    
    optim_ts(.data, test_size = 6, lag = 1, parameter = parameter, model = model)
  }
tictoc::toc()



plan(multisession(workers = 4))

tictoc::tic()
future_map(.x = c("arima", "glmnet"), .f = ~optim_ts(.data, test_size = 6, lag = 1, parameter = parameter, model = .x))
tictoc::toc()

.data <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts(method = "winsorize")



  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "arima")
  

tictoc::tic()
demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts() %>% 
  optim_ts(test_size = 6, lag = 4, model = "glmnet", parameter = parameter)
tictoc::toc()
  

.data <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts() %>% 
  get_glm(parameter = parameter) %>% 
  get_forecast_experimental(horizon = 100)

  
  get_arima(parameter = parameter) %>% 
  get_forecast_experimental(horizon = 100)


c("arima", "mlr", "glmnet") %>% 
  
  
  

  # print({
  #   cat("\nMultiple keys have been found. Sample summary:");
  #   .data_tmp %>% 
  #     group_by(key) %>% 
  #     mutate(reg_name = ifelse(reg_name == 0, NA, reg_name)) %>% 
  #     summarise(obs = n()
  #               , n_regressor = n_distinct(na.omit(unique(reg_name)))
#               , date_range = paste0(min(date_var), " / ", max(date_var))
#               , .groups = "drop") %>% 
#     slice(1:5) %>% 
#     knitr::kable(., "simple")
# })