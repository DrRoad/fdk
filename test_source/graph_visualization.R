


prescription <- attributes(.data)[["prescription"]]
impute_method <- c("kalman", "winsorize", "median")

.data <- data_all %>% 
  filter(key == "FI: 592905") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = c("kalman", "winsorize", "median")) %>% 
  mutate_at(.vars = vars(prescription$reg_names)
            , .funs = ~ifelse(.x != 0, y_var, NA_real_))
  

cols <- c("y_var_kalman" ="red"
          , "y_var_winsorize" = "green"
          , "y_var_median" = "blue"
          , "y_var" = "black")

.data_l <- .data %>%
  select(-trend, -seasonal_var) %>% 
  pivot_longer(cols = 3:last_col())


clean_data <- .data_l %>% 
  filter(str_detect(name, "y_var")) %>% 
  mutate(name = fct_relevel(name, "y_var", after = Inf))

clean_g <- clean_data %>% 
  ggplot()+
  geom_line(aes(date_var, value, col = name)
            , size = 1.1)+
  scale_color_manual(values = cols)

reg_data <- .data_l %>% 
  filter(name %in% prescription$reg_names) %>% 
  rowwise() %>% 
  mutate(label = ifelse(is.na(value), NA_character_, name))

g3 <- clean_g +
  geom_point(aes(date_var, value), data = reg_data)+
  geom_text(aes(date_var, value, label = label), data = reg_data)


ma <- clean_data %>% 
  filter(name == "y_var") %>% 
  mutate(ma = slider::slide_dbl(.x = value, .f = ~mean(.x), .before = 10, .step = 1))


g4 <- g3+
  geom_smooth(method = "lm", formula = y_var ~ splines::bs(date_var, 3), se = FALSE)
  geom_line(aes(date_var, ma), data = ma, col="purple")



impute_method <- c("kalman", "winsorize", "median")
impute_cols <- c("#ff764a", "#0bdb00", "#007bff", "#8410D1E6")
#impute_cols <- c("green", "red", "purple")
impute_col_count <- 0
op <- c("y_var_kalman"="#ff764a", "y_var_winsorize"="#0bdb00", "y_var_median"="#007bff")


base_g <- .data %>% 
  ggplot()

base_g +
  geom_line(aes(x = date_var
                , y = y_var_kalman, color = impute_cols[impute_col_count]))+
  geom_line(aes(x = date_var
                , y = y_var_winsorize, color = impute_cols[impute_col_count+1]))+
  geom_line(aes(date_var, y_var), size = 1.1)+
  scale_color_manual(breaks = c("y_var_kalman","y_var_winsorize")
                     , values = c("y_var_kalman" = "blue", "y_var_winsorize" = "red")
                       #, labels = c("kalman", "winsorize"
                     )


for(i in impute_method){
  impute_col_count <- impute_col_count + 1
  base_g <- base_g +
    geom_line(aes(x = date_var
                  , y = !!sym(paste0("y_var_", i))
                  , color = impute_cols[impute_col_count]
                  #, color = impute_cols[impute_col_count])
                  #, col = as.character(impute_col_count))
                  #, col = as.character(impute_cols[impute_col_count]))
              , size = 1.1))+
    geom_line(aes(date_var, y_var), size = 1.1)+
    theme_minimal()
  for(j in prescription$reg_names){
    base_g <- base_g +
      geom_point(aes(date_var, !!sym(j)), size = 3)
  }
}

base_g+
  #scale_color_identity(guide = "legend")
  scale_color_manual(values = op)



map(impute_method, .f = function(x){
  base_g <<- base_g +
    geom_line(aes(date_var, !!sym(paste0("y_var_", x))), col = "red")
  
  base_g <<- base_g +
    geom_line(aes(date_var, y_var))
})

map(prescription$reg_names, .f = function(x){
  base_g <<- base_g +
    geom_point(aes(date_var, !!sym(x)))
  
  base_g <<- base_g +
    geom_line(aes(date_var, y_var))
})

base_g+
  geom_line(aes(date_var, y_var_kalman), col = "red")+
  geom_line(aes(date_var, y_var))







  geom_point(aes(date_var, fi_592905_other1))+
  geom_line(aes(date_var, y_var))

