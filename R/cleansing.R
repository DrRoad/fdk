
# Imputation methods ------------------------------------------------------

#' Winsorize imputation
#'
#' This function replaces outliers by an expected value given the 5% and 95% percentiles of the
#' error component plus the denoised series.
#' @param y_var Numeric. Time series to be cleansed.
#' @param na_marker logical or binary indicator that defines which observation should be take out of the
#' loess decomposition.
#' @param frequency Numeric. Time series frequency, 12 by default.
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
na_winsorize <- function(y_var, na_marker=NULL, frequency = 12, print_all = FALSE){
  if(is.null(na_marker)==FALSE){
    y_var_na <- y_var
    y_var_na[na_marker==1] <- NA
  } else {
    y_var_na <- y_var
  }
  
  y_var_decomp <- as_tibble(stlplus(y_var_na, n.p = frequency
                                     , s.window = "periodic", )[["data"]][,c(1:4)]) %>% 
    mutate(original = as.numeric(y_var)
           , y_var_denoise = seasonal + trend)
  
  tmp_out <- y_var_decomp %>% 
    mutate(remainder_winso = case_when(
             remainder < quantile(remainder, probs = 0.05, na.rm = T) ~ quantile(remainder, probs = 0.05, na.rm = T)
             , remainder > quantile(remainder, probs = 0.95, na.rm = T) ~ quantile(remainder, probs = 0.95, na.rm = T)
             , TRUE ~ remainder)
           , thres_low = y_var_denoise + quantile(remainder, probs = 0.05, na.rm = T)
           , thres_up = y_var_denoise + quantile(remainder, probs = 0.95, na.rm = T)
           , y_var_clean = case_when(
             original > thres_up ~ thres_up
             , original < thres_low ~ thres_low
             , y_var_denoise > median(y_var_denoise) & is.na(raw) == T ~ thres_up
             , y_var_denoise < median(y_var_denoise) & is.na(raw) == T ~ thres_low
             , TRUE ~ original))
  
  if(print_all == FALSE){
    tmp_out %>% 
      .[["y_var_clean"]] %>% 
      round(1)
  } else {
    tmp_out
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
#' @param frequency Time series frequency.
#' @param replace Logical. If replace = TRUE (default) it will 
#' replace the previous y_var by its cleansed/imputated version. Otherwise, a "y_clean" column will
#' be attached to the data matrix.
#' @param ... Other parameter
#' 
#' @import imputeTS
#' @import dplyr
#' @import stats
#' @import rlang
#' @import stlplus
#' @author Obryan Poyser
#' @return
#' @export
#'
#' @examples
impute_ts <- function(.data, y_var, method="winsorize", na_exclude = NULL, frequency = 12, replace = TRUE, ...) {
  
  imputation_switcher <- function(y_var, method, frequency = frequency, ...) {
    y_var <- ts(y_var, frequency = frequency, start = c(1, 1))
    if (method == "kalman") {
      as.numeric(na_seadec(x = y_var, algorithm = "kalman", ...))
    } else if (method == "winsorize") {
      na_winsorize(y_var = y_var, ...)
    } else if (method == "mean") {
      as.numeric(na_mean(x = y_var, option = "mean", ...))
    } else if (method == "median") {
      as.numeric(na_mean(x = y_var, option = "median", ...))
    } else if (method == "nearest") {
      as.numeric(na_locf(x = y_var, option = "locf", na_remaining = "rev", ...))
    } else if (method == "interpolation") {
      as.numeric(na_interpolation(x = y_var, option = "linear", ...))
    }
  }
  
  if(is.numeric(y_var)==TRUE){
    return(imputation_switcher(y_var, method = "winsorize", frequency = frequency))
  }
  
  if(is.null(na_exclude) == FALSE) {
    na_marker_int <- rowSums(.data[setdiff(names(.data), na_exclude)])!=0
    
    if(length(method) > 1) { # many methods create a tibble with method colnames
      #message("multiple")
      tmp <- .data %>%
        mutate(
          na_marker_int = na_marker_int,
          y_var_na = ifelse(na_marker_int == 1, NA, .data[[y_var]])
        )
      
      return(
        suppressMessages(
          tmp %>%
            bind_cols(
              map(setdiff(method, "winsorize")
                  , .f = ~ imputation_switcher(
                    y_var = tmp[["y_var_na"]]
                    , method = .x
                    , frequency = frequency
                  )) %>%
                setNames(nm = paste0("y_var_", setdiff(method, "winsorize")))
              , imputation_switcher(y_var = (tmp[[y_var]])
                                    , method = "winsorize"
                                    , frequency = frequency
                                    , na_marker = na_marker_int) %>% 
                tibble(y_var_winsorize = .)
            ) %>%
            select(-y_var_na, -na_marker_int)
        )
      )
    } else if(method == "winsorize"){
      #message("winsorize unique")
      tmp <- .data %>%
        mutate(y_var_clean = imputation_switcher(
          y_var = .data[[y_var]],
          method = method,
          frequency = frequency,
          na_marker = na_marker_int)
        )
    } else if(method != "winsorize") {
      #message("non winsorize unique")
      tmp <- .data %>%
        mutate(
          na_marker_int = na_marker_int,
          y_var_clean = ifelse(na_marker_int == 1, NA, .data[[y_var]]) %>%
            imputation_switcher(y_var = .
                                , method = method
                                , frequency = frequency)
        ) %>%
        select(-na_marker_int)
    } else {
      stop("Error")
    }
    
  } else {
    message(paste0("You've selected method: {", method, "}. There are no regressors, a simple winsorize has been applied instead."))
    tmp <- .data %>%
      mutate(y_var_clean = imputation_switcher(y_var = .data[[y_var]]
                                              , method = "winsorize"
                                              , frequency = frequency)
      )
  }
  
  if (replace == TRUE) {
    tmp %>%
      mutate(!!quo({{y_var}}) := y_var_clean) %>% # masking names
      select(-y_var_clean)
  } else {
    tmp
  }
}


#' Data prescription
#' 
#' Prescribe provides a simple way to define the metadata
#'
#' @param .data DataFrame or tibble
#' @param key Column name of the key if any
#' @param y_var Column name of the variable to be forecasted
#' @param date_var Column name of time index
#' @param frequency Frequency of the data
#' @param print Logical. Print the data, TRUE by default.
#'
#' @return
#' @export
#'
#' @examples
prescribe_ts <- function(.data, key, y_var, date_var, frequency, print = TRUE){
  attr(.data, "prescription") <- list(key=key, y_var=y_var, date_var = date_var, frequency = 12
                                      , max_date = max(.data[[date_var]]))
  return(.data)
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
#' @param frequency Time series frequency.
#' @param replace Logical. If replace = TRUE (default) it will 
#' replace the previous y_var by its cleansed/imputated version. Otherwise, a "y_clean" column will
#' be attached to the data matrix.
#'
#' @return
#' @export
#'
#' @examples
clean_ts <- function(.data, y_var, date_var, method="winsorize", frequency = 12, na_exclude = NULL, replace = T){
  if(is.null(attributes(.data)[["prescription"]])==FALSE){
    message("Prescription has been used to define as meta-data")
    
    prescription <- attributes(.data)[["prescription"]]
    na_exclude <- unique(c(prescription$key, prescription$y_var, prescription$date_var, na_exclude))
    
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    frequency <- prescription$frequency
    key <- prescription$key
    
    attr(.data, "prescription")[["y_var"]] <- "y_var"
    attr(.data, "prescription")[["date_var"]] <- "date"
  }
   
  tmp <- .data %>% 
    filter(cumsum(.data[[y_var]])>0) %>% # Leading zeros
    impute_ts(y_var = y_var
              , method
              , na_exclude = na_exclude
              , frequency = frequency
              , replace = replace) %>% 
    rename(y_var = y_var, date = date_var) %>% 
    select(setdiff(names(.), na_exclude), date, y_var) %>% 
    relocate(date, .before = "y_var") %>% 
    mutate(y_var = sftools:::tscut(time_series = y_var, freq = frequency))
   
  if(replace == FALSE){
    tmp %>% 
      relocate(y_var_clean, .after = "y_var")
  } else {
    tmp
  }
}







