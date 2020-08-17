
# Imputation methods ------------------------------------------------------

#' Winsorize imputation
#'
#' This function replaces outliers by an expected value given the 5% and 95% percentiles of the
#' error component plus the denoised series.
#' @param series numeric or ts. Time series to be cleansed.
#' @param na_marker logical or binary indicator that defines which observation should be take out of the
#' loess decomposition.
#' @param frequency numeric. Time series frequency
#' @param print_all logical. Whether to print all time series components.
#'
#' @author Obryan Poyser
#' @return numeric cleansed series
#' @export
#' 
#' @importFrom stlplus stlplus
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' na_winsorize(AirPassengers)
#' }
na_winsorize <- function(series, na_marker=NULL, frequency = 12, print_all = FALSE){
  if(any(is.na(series))==T){
    series_na <- series
  } else {
    series_na <- series
    series_na[na_marker==1] <- NA
  }
  
  series_decomp <- as_tibble(stlplus(series_na, n.p = frequency, s.window = "periodic", )[["data"]][,c(1:4)]) %>% 
    mutate(original = series
           , series_denoise = seasonal + trend)
  tmp_out <- series_decomp %>% 
    mutate(index = 1:n()
           , remainder_winso = case_when(
             remainder < quantile(remainder, probs = 0.05, na.rm = T) ~ quantile(remainder, probs = 0.05, na.rm = T)
             , remainder > quantile(remainder, probs = 0.95, na.rm = T) ~ quantile(remainder, probs = 0.95, na.rm = T)
             , TRUE ~ remainder)
           , thres_low = series_denoise + quantile(remainder, probs = 0.05, na.rm = T)
           , thres_up = series_denoise + quantile(remainder, probs = 0.95, na.rm = T)
           , clean = case_when(
             original > thres_up ~ thres_up
             , original < thres_low ~ thres_low
             , abs(series_denoise - thres_low) < abs(series_denoise - thres_up) & is.na(raw) == T ~ thres_low
             , abs(series_denoise - thres_low) > abs(series_denoise - thres_up) & is.na(raw) == T ~ thres_up
             , TRUE ~ original))
  
  if(print_all == FALSE){
    tmp_out %>% 
      .[["clean"]] %>% 
      round(1)
  } else {
    tmp_out
  }
}

#' Imputation method switcher
#'
#' @param method string. Method to be applied to the time series. Options: kalman, winsorize, 
#' mean, median, nearest, interpolation.  
#' @param ... other arguments of the base functions.
#' @param .data 
#' @param y_var 
#' @param na_marker 
#' @param frequency 
#' @param replace 
#'
#' @author Obryan Poyser
#' @import imputeTS
#' @return
#' @noRd
#'
#' @examples
impute_ts <- function(.data, y_var, method, na_marker, frequency=12, replace = TRUE,...){
  
  imputation_switcher <- function(series, method, frequency=frequency, ...){
    series <- ts(series, frequency = frequency, start = c(1, 1))
    if(method == "kalman"){
      as.numeric(na_seadec(x = series, algorithm = "kalman", ...))
    } else if(method == "winsorize") {
      na_winsorize(series = series, ...)
    } else if(method == "mean") {
      as.numeric(na_mean(x = series, option = "mean", ...))
    } else if(method == "median") {
      as.numeric(na_mean(x = series, option = "median", ...))
    } else if(method == "nearest") {
      as.numeric(na_locf(x = series, option = "locf", na_remaining = "rev", ...))
    } else if(method == "interpolation") {
      as.numeric(na_interpolation(x = series, option = "linear", ...))
    }
  }
  
  
  if(missing(na_marker)==FALSE){
    
    na_marker <- .data %>% # na marker is formed as columns different from 0, thus, no reg_value
      select({{na_marker}}) %>% 
      rowSums()!=0
    
    if(length(method)==1){ # method selection, one or many
      tmp <- .data %>% 
        mutate(na_marker = na_marker
               , y_clean = ifelse(na_marker == 1, NA, {{y_var}}) %>%
                                    imputation_switcher(method = method
                                                        , frequency = frequency)) %>% 
        select(-na_marker)
      
      } else { # many methods create a tibble with method colnames
        tmp <- .data %>% 
        mutate(na_marker = na_marker
               , y_clean = ifelse(na_marker == 1, NA, {{y_var}}))
      
      suppressMessages(
        tmp %>% 
          bind_cols(
            map(method, .f = ~imputation_switcher(series = tmp[["y_clean"]]
                                                  , method = .x
                                                  , frequency = frequency)) %>% 
              bind_cols() %>% 
              setNames(nm = paste0("y_", method))
          ) %>% 
          select(-y_clean, -na_marker)
      )
    }
  } else if(missing(na_marker)==TRUE) { # Winsorize imputation if no regressors
    tmp <- .data %>% 
      mutate(y_clean = imputation_switcher(series = {{y_var}}
                                           , method = "winsorize", frequency = frequency))
  }
  
  if(replace == TRUE){ # by default, there is a substitution of the yvar
    tmp %>% 
      mutate(!!quo({{y_var}}) := y_clean) %>% # masking names
      select(-y_clean)
  } else {
    tmp
  }
}


demo_1 %>% 
  impute_ts(y_var = volume, method = "winsorize", replace = F)


# Cleansing ---------------------------------------------------------------

#' Cleansing function
#' 
#' This function collects the data and applies imputation, time series cuts and leading zeros filtering
#'
#' @param data tibble or data frames
#' @param method string. Method to be applied to the time series. Options: kalman, winsorize, 
#' mean, median, nearest, interpolation
#' @param frequency time series frequency
#' @param regressors string. vector of regressors used to mark as NA's to apply cleansing methods.
#' @param keep_old logical. Keep or not the old dependent variable (time series)
#'
#' @return
#' @export
#'
#' @examples
cleansing <- function(data, method, frequency = 12, regressors = NULL, keep_old = FALSE){
  
  if(is.null(regressors)==F & method == "winsorize"){
    na_marker <- rowSums(data[regressors])==0
    tmp_out <- data %>% 
      filter(cumsum(y)>0) %>% 
      mutate(y_clean = imputation_switcher(series = y, method = method
                                           , frequency = frequency, na_marker = na_marker)
             , .after = "y")
  } else {
    tmp_out <- data %>% 
      filter(cumsum(y)>0) %>% 
      mutate(y_clean = imputation_switcher(series = y, method = method, frequency = frequency)
             , .after = "y")
  }
  
  if(keep_old == TRUE){
    tmp_out
  } else {
    tmp_out %>% 
      select(-y) %>% 
      rename(y = y_clean) %>% 
      select(y, everything())
  }
}





