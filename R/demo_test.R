
sales <- sftools:::rdata2obj("C:/Users/I0415596/Documents/Projects/Functional workflow/01_monthly_data/data/Outputs/Nordics/Aug - 2020/full_sales.RData")
regressors <- sftools:::rdata2obj("C:/Users/I0415596/Documents/Projects/Functional workflow/01_monthly_data/data/Outputs/Nordics/Aug - 2020/causal_factor.RData")
reg_table <- sftools:::rdata2obj("C:/Users/I0415596/Documents/Projects/Functional workflow/01_monthly_data/data/Outputs/Nordics/Aug - 2020/reg_table.RData") %>% 
  select(forecast_item, reg_name, reg_date, reg_value)
model_param <- sftools:::rdata2obj("C:/Users/I0415596/Documents/Projects/Functional workflow/01_monthly_data/data/Outputs/Nordics/Aug - 2020/model_param.RData")


demo_items <- c("NO: 389246", "NO: 249007", "SE: 293520", "SE: 492598", "SE: 652383"
                , "DK: 155157", "DK: 578281", "FI: 515188", "FI: 34142", "FI: 592905")
data_with_reg <- demo_data %>% 
  group_by(forecast_item) %>% 
  summarise(has_reg = n_distinct(reg_name)-1)

demo_data <- sales %>% 
  select(forecast_item, date, volume=quantity) %>% 
  left_join(reg_table, by = c("forecast_item", "date"="reg_date")) %>% 
  replace_na(replace = list(reg_name = 0, reg_value = 0)) %>% 
  filter(forecast_item %in% demo_items) %>% 
  group_by(forecast_item) %>% 
  filter(cumsum(volume)>0) %>% 
  ungroup() %>% 
  unique()

saveRDS(demo_data, file = "demo_data_update.rds")


demo_data <- readRDS("demo_data_update.rds")

demo_1 <- demo_data %>% 
  filter(forecast_item == "FI: 592905") %>% 
  pivot_wider(names_from = reg_name, values_from = reg_value) %>% 
  mutate_at(.vars = vars(4:last_col()), ~ifelse(is.na(.x), 0, .x)) %>% 
  janitor::clean_names() %>% 
  select(-x0)

yvar <- demo_1$volume
na_marker <- rowSums(demo_1[c("fi_592905_other1", "fi_592905_competitor_oos1", "fi_592905_oos_2")
])!=0

demo_1 %>% 
  mutate(clean = impute_internal(volume, method = "winsorize")) %>% 
  ggplot()+
  geom_line(aes(date, volume))+
  geom_line(aes(date, clean), col ="blue")


