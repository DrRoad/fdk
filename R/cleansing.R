
# Imputation methods ------------------------------------------------------

#' Winsorize imputation
#'
#' This function replaces outliers by an expected value given the 5% and 95% percentiles of the
#' error component plus the denoised series.
#' @param series numeric or ts. Time series to be cleansed.
#' @param na_marker logical or binary indicator that defines which observation should be take out of the
#' loess decomposition.
#'
#' @return numeric cleansed series
#' @export
#' 
#' @importFrom stlplus stlplus
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
#'
#' @examples na_wins
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
#' @param series 
#' @param method 
#' @param ... 
#'
#' @return
#' @noRd
#'
#' @examples
imputation_switcher <- function(series, method, frequency=12, ...){
  series <- ts(series, frequency = frequency, start = c(1, 1))
  if(method == "kalman"){
    as.numeric(imputeTS::na_seadec(x = series, algorithm = "kalman", ...))
  } else if(method == "winsorize") {
    na_winsorize(series = series, ...)
    } else if(method == "mean") {
      as.numeric(imputeTS::na_mean(x = series, option = "mean", ...))
      } else if(method == "median") {
        as.numeric(imputeTS::na_mean(x = series, option = "median", ...))
        } else if(method == "nearest") {
          as.numeric(imputeTS::na_locf(x = series, option = "locf", na_remaining = "rev", ...))
          } else if(method == "interpolation") {
            as.numeric(imputeTS::na_interpolation(x = series, option = "linear", ...))
            }
}


# Cleansing ---------------------------------------------------------------

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
