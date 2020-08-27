
# Imputation methods ------------------------------------------------------

#' Winsorize imputation
#'
#' This function replaces outliers by an expected value given the 5% and 95% percentiles of the
#' error component plus the denoised series.
#' @param y_var Numeric. Time series to be cleansed.
#' @param na_marker logical or binary indicator that defines which observation should be take out of the
#' loess decomposition.
#' @param freq Numeric. Time series frequency, 12 by default.
#' @param include Logical. Whether to print all time series components.
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
#' na_winsorize(AirPassengers)
#' }
na_winsorize <- function(y_var, na_marker=NULL, freq = 12, include = FALSE){
  
  y_var_denoise <- NULL
  
  if(is.null(na_marker)==FALSE){
    y_var_na <- y_var
    y_var_na[na_marker==1] <- NA
  } else {
    y_var_na <- y_var
  }
  
  y_var_decomp <- as_tibble(stlplus(y_var_na, n.p = freq
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
  
  if(include == FALSE){
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
#' @return
#' @export
#'
#' @examples
impute_ts <- function(.data, y_var, method="winsorize", na_exclude = NULL, freq = 12, replace_y_var = TRUE, ...) {
  
  imputation_switcher <- function(y_var, method, freq = freq, ...) {
    y_var <- ts(y_var, frequency = freq, start = c(1, 1))
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
    return(imputation_switcher(y_var, method = "winsorize", freq = freq))
  }
  
  if(is.null(na_exclude) == FALSE) {
    na_marker_int <- rowSums(.data[setdiff(names(.data), na_exclude)])!=0
    
    if(length(method) > 1) { # many methods create a tibble with method colnames
      message("Multiple cleansing methods applied...")
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
                    , freq = freq
                  )) %>%
                setNames(nm = paste0("y_var_", setdiff(method, "winsorize")))
              , imputation_switcher(y_var = (tmp[["y_var"]])
                                    , method = "winsorize"
                                    , freq = freq
                                    , na_marker = na_marker_int) %>% 
                tibble(y_var_winsorize = .)
            ) %>%
            select(-y_var_na, -na_marker_int)
        )
      )
    } else if(method == "winsorize"){ # Not working correctly
      message("Method: winsorize has been applied to clean the time series.")
      tmp <- .data %>%
        mutate(y_var_clean = imputation_switcher(
          y_var = .data[["y_var"]],
          method = "winsorize",
          freq = freq,
          #na_marker = na_marker_int
          )
        )
    } else if(method == "none") {
      #message("none")
      tmp <- .data %>%
        mutate(y_var_clean = y_var)
    } else if(method != "winsorize") {
      message(paste0("Method ", method, " has been applied to clean the time series."))
      tmp <- .data %>%
        mutate(
          na_marker_int = na_marker_int,
          y_var_clean = ifelse(na_marker_int == 1, NA, .data[["y_var"]]) %>%
            imputation_switcher(y_var = .
                                , method = method
                                , freq = freq)
        ) %>%
        select(-na_marker_int)
    } else {
      stop("ERROR")
    }
  } else {
    message(paste0("You've selected method: {", method, "}. There are no regressors, a simple winsorize has been applied instead."))
    tmp <- .data %>%
      mutate(y_var_clean = imputation_switcher(y_var = .data[[y_var]]
                                              , method = "winsorize"
                                              , freq = freq)
      )
    }
  
  if (replace_y_var == TRUE) {
    tmp %>%
      mutate(!!quo({{y_var}}) := y_var_clean) %>% # masking names
      select(-y_var_clean)
  } else {
    tmp %>%
      relocate(y_var_clean, .after = "y_var")
  }
}


#' Data prescription
#' 
#' Prescribe provides a simple way to define the metadata
#'
#' @param .data DataFrame or tibble.
#' @param key Column name of the key if any.
#' @param y_var Column name of the variable to be forecasted.
#' @param date_var Column name of time index.
#' @param freq Frequency of the data.
#'
#' @return
#' @export
#'
#' @examples
prescribe_ts <- function(.data, key, y_var, date_var, reg_name = NULL, reg_value=NULL, freq){
  attr(.data, "prescription") <- list(key = "key", y_var = "y_var"
                                      , date_var = "date_var", freq = "freq"
                                      , max_date = as.Date("1970-01-01"))
  .data_tmp <- .data %>% 
    rename("key" = key, "y_var" = y_var, "date_var" = date_var)
  
  attr(.data_tmp, "prescription")[["y_var"]] <- "y_var"
  attr(.data_tmp, "prescription")[["freq"]] <- freq
  attr(.data_tmp, "prescription")[["date_var"]] <- "date_var"
  attr(.data_tmp, "prescription")[["key"]] <- "key"
  attr(.data_tmp, "prescription")[["max_date"]] <- max(.data_tmp[["date_var"]])
  attr(.data_tmp, "prescription")[["multiple_keys"]] <- n_distinct(.data_tmp[["key"]])>1
  
  prescription <- attributes(.data_tmp)[["prescription"]]
  
  if(is.null(reg_name) == FALSE & is.null(reg_value) == FALSE){
    .data_tmp <- .data_tmp %>% 
      rename("reg_name" = reg_name, "reg_value" = reg_value)
    attr(.data_tmp, "prescription")[["reg_name"]] <- "reg_name"
    attr(.data_tmp, "prescription")[["reg_value"]] <- "reg_value"
    attr(.data_tmp, "prescription")[["has_regressors"]] <- TRUE
    prescription <- attributes(.data_tmp)[["prescription"]]
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
#' @return
#' @export
#'
#' @examples
clean_ts <- function(.data, y_var, date_var, method="winsorize", freq = 12, na_exclude = NULL, replace_y_var = TRUE){
  if(is.null(attributes(.data)[["prescription"]])==FALSE){
    prescription <- attributes(.data)[["prescription"]]
    na_exclude <- unique(c(prescription$key
                           , prescription$y_var
                           , prescription$date_var
                           , c("trend", "seasonal_var")
                           , na_exclude))
    
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    key <- prescription$key
  }
   
  .data %>%
    dplyr::filter(cumsum(.data[["y_var"]])>0) %>% # Leading zeros
    impute_ts(y_var = y_var
              , method = method
              , na_exclude = na_exclude
              , freq = freq
              , replace_y_var = replace_y_var) %>%
    mutate(trend = 1:n()
           #y_var = tscut(time_series = y_var, freq = freq) # May generate a problem
           #        , 
           )
}







