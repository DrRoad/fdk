# Cleansing ---------------------------------------------------------------

demo_2 %>%
  cleansing(yvar = "volume",method = "kalman", na_exclude = c("forecast_item", "date", "volume"), replace = F) %>% 
  ggplot()+
  geom_line(aes(date, yvar))+
  geom_line(aes(date, yvar_clean), col = "red")




parameter <- list(arima = list(order = c(0, 1, 0), seasonal = list(order = c(1, 0, 0), period = 12)))

.model_output <- demo_1 %>% 
  get_arima(yvar = volume, parameter = parameter) %>% 
  get_forecast(horizon = 20)



demo_1 %>% 
  impute_ts(yvar = "volume")










na_marker_int <- function(.data, na_exclude){
  .data %>% # na marker is formed as columns different from 0, thus, no reg_value
    select(-{{ na_exclude }}) %>% # exclude rule to the data.frame
    rowSums() != 0
}


rowSums(.data[setdiff(names(.data), na_exclude)])!=0


na_marker_int(demo_1, na_exclude = na_exclude)

na_exclude = c("forecast_item", "date", "volume")


impute_ts(.data = demo_1, yvar = "volume", method = NULL, na_exclude = na_exclude, frequency = 12, replace = F) %>% 
  ggplot()+
  geom_line(aes(date, yvar_clean), col = "blue")+
  geom_line(aes(date, volume))


imputation_switcher(yvar = pull(select(.data, volume)), method = "winsorize")









# Fit ---------------------------------------------------------------------

grid <- expand_grid(time_weight = seq(from = 0.5, to = 1, by = 0.02)
                    , trend_discount = seq(from = 0.5, to = 1, by = 0.02)
                    , alpha = seq(from = 0.5, to = 1, by = 0.10))

parameter <- list(glmnet = list(time_weight = 0.9, trend_discount = .9, alpha = 0, lambda = 0
                                , grid = grid
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.2)))

random_grid <- sample(x = 1:nrow(grid)
                      , size = round(length(1:nrow(grid))*parameter$glmnet$job$random_search_size)
                      , replace = FALSE)

splits <- split_ts(.data, test_size = 6, lag = 4)
split_1 <- splits[[1]]

.fit_output <- get_glmnet(.data = split_1[["train"]], parameter = parameter) %>% 
get_forecast_experimental(horizon = 12)






.fit_output <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts(method = c("winsorize")) %>%
  get_glmnet(parameter = parameter)



.data <- demo_2 %>% 
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date", freq = 12) %>% 
  clean_ts(method = c("winsorize")) %>%
  get_glmnet(parameter = parameter) %>%
  get_forecast_experimental(horizon = 40)




