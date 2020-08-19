tibble(a=na_winsorize(demo_1$volume, na_marker = na_marker_int)
       , b = na_winsorize(demo_1$volume)
       , c = imputation_switcher(yvar = demo_1$volume, method = "winsorize")
       , d = imputation_switcher(yvar = tmp$yvar_na, method = "winsorize")
       , init=demo_1$volume, index = 1:75) %>% 
  ggplot()+
  geom_line(aes(index, a), col ="blue")+
  geom_line(aes(index, b), col ="red")+
  geom_line(aes(index, c), col ="green")+
  geom_line(aes(index, d), col ="purple")+
  geom_line(aes(index, init))




l1 <- demo_1 %>% 
  impute_ts(yvar = volume, replace = F)

l2 <- demo_1 %>% 
  impute_ts(yvar = volume, na_exclude = c("volume", "date", "forecast_item")
            , method = "winsorize", replace = F)

l3 <- demo_1 %>% 
  impute_ts(yvar = volume, na_exclude = c("volume", "date", "forecast_item")
            , method = c("winsorize", "kalman", "mean"), replace = F)


l1 %>% 
  ggplot()+
  geom_line(aes(date, y_clean), col = "blue")+
  geom_line(aes(date, volume), col = "black")

l2 %>% 
  ggplot()+
  geom_line(aes(date, y_clean), col = "blue")+
  geom_line(aes(date, volume), col = "black")

l3 %>% 
  ggplot()+
  geom_line(aes(date, yvar_winsorize), col = "red")+
  geom_line(aes(date, yvar_kalman), col = "blue")+
  geom_line(aes(date, yvar_mean), col = "green")+
  geom_line(aes(date, volume))


l2 <- demo_1 %>% 
  impute_ts(yvar = volume, na_marker = "fi_592905_other1"
            , method = "winsorize", replace = T)



l1 %>% 
  left_join(
    l2, by = "date"
  ) %>% 
  ggplot()+
  geom_line(aes(date, volume.x))+
  geom_line(aes(date, volume.y), col = "red")+
  geom_line(aes(date, y_clean), col = "blue")


# Cleansing ---------------------------------------------------------------

demo_1 %>% 
  cleansing(yvar = volume, method = "kalman", na_exclude = c("forecast_item", "date", "volume"), replace = F)




parameter <- list(arima = list(order = c(0, 1, 0), seasonal = list(order = c(1, 0, 0), period = 12)))

.model_output <- demo_1 %>% 
  get_arima(yvar = volume, parameter = parameter) %>% 
  get_forecast(horizon = 20)
















