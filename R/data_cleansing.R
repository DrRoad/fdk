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
winsorize_ts <- function(.data, freq = numeric(), add_transformations = FALSE
                         , na_regressor = TRUE
                         , na_missing_dates = FALSE
                         , threshold = 0.05, impute_winsorize = FALSE){

  key_int <- attributes(.data)[["key"]]
  
  # Warnings ----------------------------------------------------------------

  stopifnot(exists(".log"))

  # NA rules ----------------------------------------------------------------

  if(all(na_regressor == T & na_missing_dates == T)){
    na_vec <- c(.log[[key_int]]$dates_check$dates_with_reg
                , .log[[key_int]]$dates_check$missing_dates) %>% 
      as.Date()
  } else if(na_regressor == T){
    na_vec <- c(.log[[key_int]]$dates_check$dates_with_reg) %>% 
      as.Date()
  } else if(na_missing_dates == T){
    na_vec <- c(.log[[key_int]]$dates_check$missing_dates) %>% 
      as.Date()
  } else {
    na_vec = date()
  }
  
  # TS vector ---------------------------------------------------------------

  y_var_vec <- .data_tmp %>% 
    mutate(y_var_na_reg = case_when(
      date_var %in% na_vec ~ NA_real_
      , TRUE ~ y_var)) %>% 
    pull(y_var_na_reg)

  # Decomposition -----------------------------------------------------------

  y_var_decomp <- stlplus::stlplus(x = y_var_vec, n.p = freq, s.window = "periodic")[["data"]][,c(1:4)] %>% 
    #as_tibble() %>% # slightly improvement
    mutate(y_var_denoise = seasonal + trend) %>%
    rename(trend_smooth = trend) %>% 
    mutate(remainder_winso = case_when(
      remainder < quantile(remainder, probs = threshold, na.rm = T) ~ quantile(remainder, probs = threshold, na.rm = T)
      , remainder > quantile(remainder, probs = (1 - threshold), na.rm = T) ~ quantile(remainder, probs = (1 - threshold), na.rm = T)
      , TRUE ~ remainder)
      , thres_low = y_var_denoise + quantile(remainder, probs = threshold, na.rm = T)
      , thres_up = y_var_denoise + quantile(remainder, probs = (1 - threshold), na.rm = T)
      , y_var_clean = case_when(
        raw > thres_up ~ thres_up
        , raw < thres_low ~ thres_low
        , TRUE ~ raw)
      , y_var_winso_imp = case_when(
        y_var_denoise > median(y_var_denoise) & is.na(raw) == T ~ thres_up
        , y_var_denoise < median(y_var_denoise) & is.na(raw) == T ~ thres_low
        , TRUE ~ raw)) %>% 
    dplyr::select(y_var_clean, y_var_winso_imp, y_var_denoise)
  
  if(add_transformations == FALSE){
    .data_tmp <- .data_tmp %>% 
      mutate(y_var = y_var_decomp$y_var_clean)
  } else {
    .data_tmp <- .data_tmp %>% 
      bind_cols(y_var_decomp)
  }
  
  return(.data_tmp)
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
impute_ts <- function(.data, freq = numeric()
                      , imputation_method = "none"
                      , na_regressor = TRUE
                      , na_missing_dates = TRUE
                      , replace_y_var = TRUE, ...){
  
  key_int <- attributes(.data)[["key"]]
  
  
  # Internal functions ------------------------------------------------------

  imputation_switcher <- function(.y_var, imputation_method, freq, ...) {
    y_var_int <- ts(.y_var, frequency = freq, start = c(1, 1))
    if(imputation_method == "none"){
      .y_var
    } else if (imputation_method == "kalman") {
      round(as.numeric(na_seadec(x = y_var_int, algorithm = "kalman", ...)),0)
    } else if (imputation_method == "mean") {
      as.numeric(na_mean(x = y_var_int, option = "mean", ...))
    } else if (imputation_method == "median") {
      as.numeric(na_mean(x = y_var_int, option = "median", ...))
    } else if (imputation_method == "nearest") {
      as.numeric(na_locf(x = y_var_int, option = "locf", na_remaining = "rev", ...))
    } else if (imputation_method == "interpolation") {
      as.numeric(na_interpolation(x = y_var_int, option = "linear", ...))
    } else {
      stop("Imputation method not recognized")
    }
  }
  
  # NA rules ----------------------------------------------------------------
  
  if(all(na_regressor == T & na_missing_dates == T)){
    na_vec <- c(.log[[key_int]]$dates_check$dates_with_reg
                , .log[[key_int]]$dates_check$missing_dates) %>% 
      as.Date()
  } else if(na_regressor == T){
    na_vec <- c(.log[[key_int]]$dates_check$dates_with_reg) %>% 
      as.Date()
  } else if(na_missing_dates == T){
    na_vec <- c(.log[[key_int]]$dates_check$missing_dates) %>% 
      as.Date()
  } else {
    na_vec = date()
  }

  # TS vector ---------------------------------------------------------------

  y_var_na_reg <- .data_tmp %>%
    mutate(y_var_na_reg = case_when(
      date_var %in% na_vec ~ NA_real_
      , TRUE ~ y_var)) %>% 
    pull(y_var_na_reg)
  
  # Apply imputation --------------------------------------------------------

  y_var_imp <- imputation_switcher(.y_var = y_var_na_reg
                      , imputation_method = imputation_method
                      , freq = freq)
  
  if(replace_y_var == TRUE){
    .data_tmp <- .data_tmp %>% 
      mutate(y_var = y_var_imp)
  } else {
    .data_tmp <- .data_tmp %>% 
      mutate(y_var_imp = y_var_imp, .after = "y_var")
  }
  
  return(.data_tmp)
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
clean_ts <- function(.data, freq = numeric()
                     , winsorize_config = list()
                     , imputation_config = list()){
  
  .data_tmp <- .data
  key_int <- attributes(.data)[["key"]]
  
  # Warnings ----------------------------------------------------------------
  
  if(length(freq) == 0 & exists(".log_init")){
    freq <- as.numeric(.log_init$prescription$freq)
  } else {
    stop("Please provide time series frequency.")
  }

  stopifnot(all(names(winsorize_config) %in% c("apply_winsorize"
                                               , "threshold"
                                               , "add_transformations"
                                               , "na_regressor"
                                               , "na_missing_dates"
                                               , "impute_winsorize")))
  
  stopifnot(all(names(imputation_config) %in% c("impute_method"
                                               , "replace_y_var"
                                               , "na_regressor"
                                               , "na_missing_dates")))
  
  # Default parameters ------------------------------------------------------
  
  winsorize_config_default = list(apply_winsorize = FALSE
                                  , na_regressor = FALSE
                                  , na_missing_dates = FALSE
                                  , threshold = 0.05
                                  , impute_winsorize = FALSE
                                  , add_transformations = FALSE) %>% 
    modifyList(x = ., val = winsorize_config)
  
  imputation_config_default = list(impute_method = "none"
                                   , na_regressor = FALSE
                                   , na_missing_dates = FALSE
                                   , replace_y_var = TRUE) %>% 
    modifyList(x = ., val = imputation_config)
  
  # Log update --------------------------------------------------------------
  
  log_update(module = "winsorize"
             , key = key_int
             , new_log = winsorize_config_default)
  log_update(module = "imputation"
             , key = key_int
             , new_log = imputation_config_default)
  
  # Skip leading zeros ------------------------------------------------------

  .data_tmp <- .data_tmp %>%
    dplyr::filter(cumsum(replace_na(y_var, 0))>0) %>% 
    mutate(trend = 1:n())
  
  # Imputation
  
  if(winsorize_config_default$apply_winsorize == TRUE){
    .data_tmp <- winsorize_ts(.data = .data_tmp
                              , freq = freq
                              , add_transformations = winsorize_config_default$add_transformations
                              , na_regressor = winsorize_config_default$na_regressor
                              , na_missing_dates = winsorize_config_default$na_missing_dates
                              , threshold = winsorize_config_default$threshold
                              , impute_winso = winsorize_config_default$impute_winsorize)
  }
  
  if(imputation_config_default$impute_method != "none"){
    .data_tmp <- impute_ts(.data = .data_tmp
              , imputation_method = imputation_config_default$impute_method
              , na_regressor = imputation_config_default$na_regressor
              , na_missing_dates = imputation_config_default$na_missing_dates
              , replace_y_var = imputation_config_default$replace_y_var
              , freq = freq)
  }
  
  return(.data_tmp)
}
