
# Source ------------------------------------------------------------------

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


# Data import -------------------------------------------------------------

.data0 <- read_csv("demo_data.csv") %>% 
  dplyr::filter(date < "2020-02-01") #%>% 
  #select(-reg_name, -reg_value)

.data_1 <- .data0 %>%
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value")

.data <- .data_1 %>% 
  filter(key == "FI: 34142") %>% 
  feature_engineering_ts() %>% 
  clean_ts()



model_list <- c("arima", "glmnet", "glm", "tbats", "seasonal_naive"
                , "neural_network", "croston")

model_list <- "neural_network"


l <- autoforecast(.data = .data, horizon = 100
            , model = c("glm", "glmnet", "neural_network", "arima", "ets"
                        , "seasonal_naive", "tbats", "croston")
            , parameter = parameter, optim_profile = "fast")

l1 <- autoforecast(.data = .data, horizon = 100
                  , model = c("glm", "glmnet", "neural_network", "arima", "ets"
                              , "seasonal_naive", "croston")
                  , parameter = parameter, optim_profile = "light", test_size = 6, lag = 3)

l1 %>% 
  print_ts(interactive = T)


tictoc::tic()
autoforecast(.data = .data, horizon = 100
             , model = c("glm", "glmnet", "neural_network", "arima", "ets"
                         , "seasonal_naive", "tbats", "croston")
             , parameter = parameter, optim_profile = "light", test_size = 6, lag = 3)
tictoc::toc()




# Test --------------------------------------------------------------------

optim_light <- .data_1 %>% 
  filter(key == "FI: 34142") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize") %>% 
  optim_ts(test_size = 6, lag = 3, parameter = parameter
           , model = model_list)


fit_ts(.data = .data, parameter = update_parameter(parameter, optim_light$parameter[[1]]
                                                   , model = "glmnet")
       , model = "glmnet") %>% 
  get_forecast(horizon= 100)





best_model <- optim_ts(.data, test_size = 6, lag = 3, parameter = parameter, model = model_list)
best_model_table <- best_model
model <- "glmnet"





optim_join <- function(.data, model, parameter, horizon, best_model){
  if(model %in% c("glmnet", "glm")){ # missing arima
    get_forecast_int(.data, model = model
                     , parameter = update_parameter(parameter
                                                    , best_model_table$parameter[[which(best_model_table$model==model)]]
                                                    , model = model, optim = TRUE)
                     , horizon = 100)
  } else {
    get_forecast_int(.data, model = model, parameter = parameter, horizon = horizon)
  }
}

optim_join(.data, "seasonal_naive", parameter, 100, best_model)



forecast_tmp <- map(model_list, ~optim_join(.data, model = .x, parameter = parameter, horizon = 100, best_model)) %>% 
  bind_rows() %>% 
  rename(date_var = date, y_var = y_var_fcst) %>% 
  mutate(type = "forecast") %>% 
  bind_rows(.data_tmp, .) %>% 
  select(key:y_var, model, type) %>% 
  replace_na(replace = list(type = "history", model = "history"))

ensemble_tmp <- forecast_tmp %>% 
  filter(type != "history") %>% 
  group_by(date_var) %>% 
  summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")

forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp)

attr(forecast_tmp, "output_type") <- "optim_output"


optim_join(.data, model = "glmnet"
           , parameter = parameter
           , horizon = 10)




which(light_optim_best_par$model=="glmnet")


fast_optim_int(.data, model = "glmnet"
               , parameter = update_parameter(parameter
                                              , light_optim_best_par$parameter[[which(light_optim_best_par$model=="glmnet")]]
                                              , "glmnet")
               , horizon = 100)






















new_parameter <- optim_light$parameter[[1]]
old_parameter <- parameter


update_parameter(old_parameter = parameter, new_parameter = new_parameter, model = "glmnet")








.data <- .data_1 %>% 
  filter(key == "FI: 34142")



.data_1 %>% 
  filter(key == "FI: 34142") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize") %>% 
  optim_ts(test_size = 6, lag = 3, parameter = parameter
           , model = model_list)
tictoc::toc()



.forecast <- .data_1 %>% 
  filter(key == "DK: 578281") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize") %>% 
  fit_ts(model = "tbats", parameter = parameter) %>% 
  get_forecast(horizon = 100)


optim_profile <- c("fast", "light", "medium", "complete")



.data <- .data %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "winsorize")


.data %>% 
  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "ets")

l %>% 
  filter(type != "history") %>% 
  group_by(date_var) %>% 
  summarise(y_var = mean(y_var), model = "ensemble", type = "forecast")














l1 %>% 
  print_ts(interactive = T)






l <- forecast_ts(.data = .data, horizon = 100
            , model = c("glm", "glmnet", "arima", "ets", "seasonal_naive", "tbats", "croston")
            , parameter = parameter, optim_profile = "fast")

print_ts <- function(.data, interactive = FALSE){
  if(attributes(.data)[["output_type"]] != "optim_output"){
    stop("Error, the input data is not class optim_output")
  } else {
    graph_tmp <- .data %>% 
      ggplot(aes(date_var, y_var, col = model))+
      geom_line()+
      labs(x = "", y = "y_var")+
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      scale_x_date(date_breaks = "6 month", minor_breaks = NULL)+
      theme_minimal()+
      theme(axis.text = element_text(size = 12)
            , axis.text.x = element_text(angle = 90))
    
    if(interactive == TRUE){
      plotly::ggplotly(graph_tmp)
    } else {
      graph_tmp
    }
  }
}




.data <- .data_1 %>% 
  filter(key == "DK: 578281") 







%>% 
  clean_ts(method = "kalman") %>% 
  get_ets() %>% 
  get_forecast(horizon = 100)

.data <- .data_1$data[[1]] %>% 
  clean_ts(method = "kalman") %>% 
  optim_ts(test_size = 6, lag = 3, model = c("arima", "glmnet"), parameter = parameter)


pkg <- c("magrittr", "dplyr", "stlplus", "glmnet", "forecast")


foreach(key=seq_along(.data_1$key), .packages = pkg) %dopar% {
  .data_1$data[[key]] %>% 
    clean_ts(method = "winsorize") %>% 
    get_ets() %>% 
    get_forecast(horizon = 100)
}


function



.data_1$data[[1]] %>% 
  clean_ts(method = c("kalman", "winsorize")) %>% 
  ggplot(aes(date_var, y_var))+
  geom_line()






















.data <- .data$data[[1]]

.data %>% 
  clean_ts() %>% 
  View()


t1$data[[1]] %>% 
  pivot_wider(names_from = "reg_name", values_from = "reg_value") %>% 
  select(-matches("0|NA$")) %>% 
  janitor::clean_names() %>% 
  mutate_at(.vars = vars(-matches("date_var|key|y_var")), .funs = ~ifelse(is.na(.x), 0, .x)) %>% 
  get_design_matrix()





keep_reg <- sum(t1$data[[1]][["reg_value"]])>0

if(keep_reg == FALSE){
  x %>% 
    select(-reg_value, -reg_name)
} else {
  x
}
t1$data[[1]] %>% 
  select_if(sum(reg_value))

sum(unique(t1$data[[1]]$reg_name)!="0")







.data_tmp %>% 
  group_by(key) %>% 
  mutate(reg_name = ifelse(reg_name == 0, NA, reg_name)) %>% 
  summarise(obs = n()
            , n_regressor = n_distinct(na.omit(unique(reg_name)))
            , date_range = paste0(min(date), " / ", max(date))
            , .groups = "drop") %>% 
  knitr::kable(., "simple")


cat(paste0("#", unique(.data_tmp$key)))
