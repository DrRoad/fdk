
# cleansing

cleansing <- function(data, regressors = "reg", method = "kalman", keep_old = FALSE){
  # Impute function
  impute_int <- function(x, method){
    ts_tmp <- ts(x, frequency = 12, start = c(1, 1))
    imputeTS::na_seadec(x = ts_tmp, algorithm = method) %>% 
      as.numeric()
  }
  data[["reg_ind"]] <- data[regressors] %>% 
    rowSums()
  # Filtering
  tmp <- data %>% 
    # rename(y = 1) %>% 
    mutate(y_clean = case_when(
      reg_ind !=0 ~ NA_real_
      , TRUE ~ y)
      , y_clean = impute_int(y_clean, method = method))
  if(keep_old == TRUE){
    tmp %>% 
      select(-reg_ind) %>% 
      relocate(y_clean, .after = y)
  } else {
    tmp %>% 
      select(-y) %>% 
      rename(y = y_clean) %>% 
      select(y, everything(), -reg_ind) %>%
      # Clean 0's
      filter(cumsum(y)>0)
  }
}
