

dk_sample <- import_data(source_conf = source_conf)

so_data <- readxl::read_excel("C:/Users/I0415596/Desktop/Book1.xlsx") %>% 
  pivot_longer(cols = 3:last_col()) %>% 
  rename(reg_name = 1, reg_value = value) %>% 
  mutate(date = as.Date(as.numeric(name), origin = "1899-12-30")
         , forecast_item = paste0("DK: ", forecast_item)
         , reg_name = paste0("so_", str_extract(reg_name, "[A-Za-z]+"))) %>% 
  dplyr::select(-name) %>% 
  na.omit()

sample_data <- dk_sample$sales %>% 
  filter(forecast_item %in% unique(so_data$forecast_item)) %>% 
  bind_rows(so_data) %>% 
  prescribe_ts(key = "forecast_item", y_var = "sales", reg_name = "reg_name"
               , reg_value = "reg_value", date_var = "date", freq = 12
               , date_format = "ymd") %>% 
  mutate(reg = map_chr(data, ~{
    .x %>% 
      dplyr::filter(str_detect(reg_name, "so_")) %>% 
      pull(reg_name) %>% 
      unique() %>% 
      janitor::make_clean_names()
  }))


get_results <- function(data, reg, include_so = T){
  lag_var_in <- setNames(list(3), reg)
  parameter <- get_default_hyperpar()
  exc <- c(reg)
  
  if(include_so == F){
    exc <- c(exc, paste0(reg, "_lag_3"))
  }
  
  data %>% 
    validate_ts() %>% 
    feature_engineering_ts(lag_var = lag_var_in) %>% 
    #filter(history == 1) %>% 
    dplyr::select(-any_of(c("history", exc))) %>% 
    optim_ts(ts_model = "glmnet"
             , optim_conf = get_default_optim_conf()
             , parameter = parameter
             , export_fit = F)
}

sample_data2 <- sample_data %>% 
  mutate(r1 = map2(data, reg, ~get_results(.x, .y, include_so = F))
         , r2 = map2(data, reg, ~get_results(.x, .y, include_so = T)))

sample_data2 %>% 
  dplyr::select(key, r1, r2) %>% 
  mutate(across(.cols = c(r1, r2)
                , .fns = ~map(.x, ~dplyr::select(.x, model, mape, spa) %>% 
                                slice(1)
                ))) %>% 
  rename(wo_so = r1, w_so = r2) %>% 
  pivot_longer(cols = 2:3) %>% 
  unnest(value) %>% 
  pivot_wider(names_from = name, values_from = c("mape", "spa")) %>% 
  rowwise() %>%
  mutate(across(.cols = matches("spa"), .fns = ~abs(round(1- .x, 2)))) %>% 
  mutate(mape_improv = round(mape_w_so - mape_wo_so, 2)
         , spa_improv = round(spa_w_so - spa_wo_so, 2)) %>% 
  pull(mape_improv) %>% 
  range()
  

    