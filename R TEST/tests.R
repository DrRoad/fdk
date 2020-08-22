# Cleansing ---------------------------------------------------------------

# Fit ---------------------------------------------------------------------

grid <- expand_grid(time_weight = seq(from = 0.7, to = 1, by = 0.05)
                    , trend_discount = seq(from = 0.8, to = 1, by = 0.05)
                    , alpha = seq(from = 0, to = 1, by = 0.10))

parameter <- list(glmnet = list(time_weight = 0.9, trend_discount = .9, alpha = 0, lambda = 0
                                , grid = grid
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.05
                                             , n_best_model = 1)))

parameter <- update_parameter(parameter = parameter, new = grid[1,])













tictoc::tic()
optim_ts(.data, test_size = 4, lag = 4, parameter = parameter, model = "glmnet")
tictoc::toc()



autoforecast <- function(){
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
    clean_ts() %>% 
  c("glmnet") %>% 
    map(~optim_ts(.data, test_size = 6, lag = 1, parameter = parameter, model = .x))
}








AAN
NNN
AAA
ANA
ANA
NNN

cv = .08

ets(y = y_var, )


auto.arima()

set.seed(1)

.fit_output <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts(method = "winsorize") %>% 
  get_glm(parameter = parameter)






  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "arima")
  

tictoc::tic()
demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts() %>% 
  optim_ts(test_size = 6, lag = 4, model = "glmnet", parameter = parameter)
tictoc::toc()
  

demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts() %>% 
  get_glmnet(parameter = parameter) %>% 
  get_forecast_experimental(horizon = 100)

  
  get_arima(parameter = parameter) %>% 
  get_forecast_experimental(horizon = 100)


c("arima", "mlr", "glmnet") %>% 
  
  
  

.fit_output <- tran
