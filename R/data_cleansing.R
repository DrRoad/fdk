#' Winsorize imputation
#'
#' This function replaces outliers by an expected value given the 5% and 95% percentiles of the
#' error component plus the denoised series.
#' @param y_var Numeric. Time series to be cleansed.
#' @param na_marker logical or binary indicator that defines which observation should be take out of the
#' loess decomposition.
#' @param freq Numeric. Time series frequency, 12 by default.
#' @param print_all Logical. Whether to print all time series components.
#'
#' @author Obryan Poyser
#' @return numeric cleansed series
#' @export
#' 
#' @importFrom rlang .data
#' @import stlplus
#' @import tibble
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' winsorize_ts(AirPassengers)
#' }
winsorize_ts <- function(.data, freq = 12, print_all = FALSE, threshold = 0.05, impute = FALSE){
  
  winsorize_ts_int <- function(.data, freq, print_all = FALSE, threshold = 0.05, impute = FALSE){
    y_var <- .data[["y_var"]]
    
    y_var_decomp <- as_tibble(stlplus(y_var, n.p = freq
                                      , s.window = "periodic")[["data"]][,c(1:4)]) %>% 
      mutate(original = as.numeric(y_var)
             , y_var_denoise = seasonal + trend) %>% 
      rename(trend_smooth = trend)
    
    tmp <- y_var_decomp %>% 
      mutate(remainder_winso = case_when(
        remainder < quantile(remainder, probs = threshold, na.rm = T) ~ quantile(remainder, probs = threshold, na.rm = T)
        , remainder > quantile(remainder, probs = (1 - threshold), na.rm = T) ~ quantile(remainder, probs = (1 - threshold), na.rm = T)
        , TRUE ~ remainder)
        , thres_low = y_var_denoise + quantile(remainder, probs = threshold, na.rm = T)
        , thres_up = y_var_denoise + quantile(remainder, probs = (1 - threshold), na.rm = T)
        , y_var_clean = case_when(
          original > thres_up ~ thres_up
          , original < thres_low ~ thres_low
          , TRUE ~ original))
    
    if(impute == TRUE){
      tmp <- tmp %>% 
        mutate(y_var_clean = case_when(
          y_var_denoise > median(y_var_denoise) & is.na(raw) == T ~ thres_up
          , y_var_denoise < median(y_var_denoise) & is.na(raw) == T ~ thres_low
          , TRUE ~ original)
        )
    }
    
    if(print_all == FALSE){
      tmp %>% 
        dplyr::select(y_var_clean)
    } else {
      tmp
    }
  }
  
  if(print_all == TRUE){
    .data %>% 
      bind_cols(winsorize_ts_int(.data = .data, freq = 12, print_all = print_all, impute = impute))
  } else {
    .data %>% 
      bind_cols(winsorize_ts_int(.data = .data, freq = 12, print_all = FALSE, impute = impute)) %>% 
      mutate(y_var = y_var_clean) %>% 
      dplyr::select(-y_var_clean)
  }
}

#' Time series imputation
#'
#' @param .data DataFrame or tibble.
#' @param y_var Quoted or unquoted variable name to be cleansed.
#' @param method Method to be applied to the time series. Options: kalman, winsorize, 
#' mean, median, nearest observation, interpolation. If more than one method is chosen, a set of new columns
#' is automatically generated.
#' @param na_exclude Vector of names that **should not** be used to replace the observation by NA.
#' @param freq Time series frequency.
#' @param replace_y_var Logical. If replace_y_var = TRUE (default) it will 
#' replace the previous y_var by its cleansed/imputated version. Otherwise, a "y_clean" column will
#' be attached to the data matrix.
#' @param ... Other parameters,
#' .
#' 
#' @import imputeTS
#' @import dplyr
#' @import stats
#' @import rlang
#' @import stlplus
#' @author Obryan Poyser
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' impute_ts()
#' }
impute_ts <- function(.data, method = "kalman", na_exclude = NULL, freq = NULL, replace_y_var = TRUE,...) {
  
  prescription <- attributes(.data)[["prescription"]]
  
  imputation_switcher <- function(y_var, method, freq, ...) {
    y_var <- ts(y_var, frequency = freq, start = c(1, 1))
    if (method == "kalman") {
      round(as.numeric(na_seadec(x = y_var, algorithm = "kalman", ...)), 2)
    } else if (method == "mean") {
      as.numeric(na_mean(x = y_var, option = "mean", ...))
    } else if (method == "median") {
      as.numeric(na_mean(x = y_var, option = "median", ...))
    } else if (method == "nearest") {
      as.numeric(na_locf(x = y_var, option = "locf", na_remaining = "rev", ...))
    } else if (method == "interpolation") {
      as.numeric(na_interpolation(x = y_var, option = "linear", ...))
    } else {
      stop("Imputation method not recognized")
    }
  }
  
  suppressMessages({
      if(all(method == "none") == FALSE){
        default_cols <- c("key", "y_var", "date_var", "trend", "seasonal_var")
        na_marker_int <- rowSums(.data[setdiff(names(.data), c(default_cols, na_exclude))])!=0
        
        tmp <- .data %>%
          mutate(
            na_marker_int = na_marker_int,
            y_var_na = ifelse(na_marker_int == 1, NA_real_, y_var)) %>% 
          {tmp2 <<- .} %>% 
          bind_cols(
            map_dfc(method, .f = ~imputation_switcher(tmp2[["y_var_na"]]
                                                      , method = .x
                                                      , freq = prescription[["freq"]])) %>% 
              setNames(nm = paste0("y_var_", method))) %>% 
          dplyr::select(-na_marker_int, -y_var_na) %>% 
          relocate(matches("y_var_"), .after ="y_var")
        
        if(replace_y_var == T & length(method)==1){
          tmp <- tmp %>% 
            mutate(y_var = !!sym(paste0("y_var_", method))) %>% 
            dplyr::select(-paste0("y_var_", method))
        }
      } else if(method =="none"){
        tmp <- .data
      }
  })
  
  return(tmp)
}

# Cleansing ---------------------------------------------------------------

#' Time Series Cleansing
#' 
#' This function collects the data and applies imputation, time series cuts and leading zeros filtering
#'
#' @param .data DataFrame or tibble.
#' @param y_var String. Colname of the variable to be cleansed.
#' @param date_var String. Colname of time serie index.
#' @param method Method to be applied to the time series. Options: kalman, winsorize, 
#' mean, median, nearest observation, interpolation. If more than one method is chosen, a set of new columns
#' is automatically generated.
#' @param na_exclude Vector of names that **should not** be used to replace the observation by NA.
#' @param freq Time series frequency.
#' @param replace_y_var Logical. If replace = TRUE (default) it will 
#' replace the previous y_var by its cleansed/imputated version. Otherwise, a "y_var_clean" column will
#' be attached to the data matrix.
#'
#' @import stats
#' @import dplyr
#' @import fastDummies
#' @return data-frame, tibble or tsibble.
#' @export
#'
#' @examples
#' \dontrun{
#' clean_ts()
#' }
clean_ts <- function(.data, method = "kalman", na_exclude = NULL, replace_y_var = TRUE
                     , print_all=FALSE, threshold = 0.05, impute = FALSE, winsorize = TRUE){
  
  prescription <- attributes(.data)[["prescription"]]
  
  tmp <- .data %>%
    dplyr::filter(cumsum(replace_na(y_var, 0))>0) %>% 
    impute_ts(method = method
              , na_exclude = na_exclude
              , freq = prescription[["freq"]]
              , replace_y_var = replace_y_var)
  
  if(winsorize == TRUE){
    tmp <- tmp %>% 
      winsorize_ts(freq = prescription[["freq"]]
                   , print_all = print_all
                   , threshold = threshold
                   , impute = impute) %>% 
      mutate(trend = 1:n()
             #y_var = tscut(time_series = y_var, freq = freq) # May generate a problem
             #        , 
      )
  }
  
  return(tmp)
}
