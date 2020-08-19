
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
na_winsorize <- function(yvar, na_marker=NULL, frequency = 12, print_all = FALSE){
  if(is.null(na_marker)==FALSE){
    yvar_na <- yvar
    yvar_na[na_marker==1] <- NA
  } else {
    yvar_na <- yvar
  }
  
  yvar_decomp <- as_tibble(stlplus(yvar_na, n.p = frequency
                                     , s.window = "periodic", )[["data"]][,c(1:4)]) %>% 
    mutate(original = as.numeric(yvar)
           , yvar_denoise = seasonal + trend)
  
  tmp_out <- yvar_decomp %>% 
    mutate(remainder_winso = case_when(
             remainder < quantile(remainder, probs = 0.05, na.rm = T) ~ quantile(remainder, probs = 0.05, na.rm = T)
             , remainder > quantile(remainder, probs = 0.95, na.rm = T) ~ quantile(remainder, probs = 0.95, na.rm = T)
             , TRUE ~ remainder)
           , thres_low = yvar_denoise + quantile(remainder, probs = 0.05, na.rm = T)
           , thres_up = yvar_denoise + quantile(remainder, probs = 0.95, na.rm = T)
           , yvar_clean = case_when(
             original > thres_up ~ thres_up
             , original < thres_low ~ thres_low
             , yvar_denoise > median(yvar_denoise) & is.na(raw) == T ~ thres_up
             , yvar_denoise < median(yvar_denoise) & is.na(raw) == T ~ thres_low
             , TRUE ~ original))
  
  if(print_all == FALSE){
    tmp_out %>% 
      .[["yvar_clean"]] %>% 
      round(1)
  } else {
    tmp_out
  }
}

#' Time series imputation
#'
#' @param .data DataFrame or tibble.
#' @param yvar Quoted or unquoted variable name to be cleansed.
#' @param method Method to be applied to the time series. Options: kalman, winsorize, 
#' mean, median, nearest observation, interpolation. If more than one method is chosen, a set of new columns
#' is automatically generated.
#' @param na_exclude Vector of names that **should not** be used to replace the observation by NA.
#' @param frequency Time series frequency.
#' @param replace Logical. If replace = TRUE (default) it will 
#' replace the previous yvar by its cleansed/imputated version. Otherwise, a "y_clean" column will
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
impute_ts <- function(.data, yvar, method, na_exclude = NULL, frequency = 12, replace = TRUE, ...) {
  
  imputation_switcher <- function(yvar, method, frequency = frequency, ...) {
    yvar <- ts(yvar, frequency = frequency, start = c(1, 1))
    if (method == "kalman") {
      as.numeric(na_seadec(x = yvar, algorithm = "kalman", ...))
    } else if (method == "winsorize") {
      na_winsorize(yvar = yvar, ...)
    } else if (method == "mean") {
      as.numeric(na_mean(x = yvar, option = "mean", ...))
    } else if (method == "median") {
      as.numeric(na_mean(x = yvar, option = "median", ...))
    } else if (method == "nearest") {
      as.numeric(na_locf(x = yvar, option = "locf", na_remaining = "rev", ...))
    } else if (method == "interpolation") {
      as.numeric(na_interpolation(x = yvar, option = "linear", ...))
    }
  }
  
  if(is.numeric(yvar)==TRUE){
    return(imputation_switcher(yvar, method = "winsorize", frequency = frequency))
  }
  
  if (is.null(na_exclude) == FALSE) {
    na_marker_int <- .data %>% # na marker is formed as columns different from 0, thus, no reg_value
      select(-{{ na_exclude }}) %>% # exclude rule to the data.frame
      rowSums() != 0
    
    if (length(method) > 1) { # many methods create a tibble with method colnames
      
      tmp <- .data %>%
        mutate(
          na_marker_int = na_marker_int,
          yvar_na = ifelse(na_marker_int == 1, NA, {{yvar}})
        )
      
      return(
        suppressMessages(
          tmp %>%
            bind_cols(
              map(setdiff(method, "winsorize")
                  , .f = ~ imputation_switcher(
                    yvar = tmp[["yvar_na"]]
                    , method = .x
                    , frequency = frequency
                  )) %>%
                setNames(nm = paste0("yvar_", setdiff(method, "winsorize")))
              , imputation_switcher(yvar = (tmp %>% pull(volume))
                                    , method = "winsorize"
                                    , frequency = frequency
                                    , na_marker = na_marker_int) %>% 
                tibble(yvar_winsorize = .)
            ) %>%
            select(-yvar_na, -na_marker_int)
        )
      )
    } else if (length(method) == 1 & method == "winsorize") { # if only one method is selected
      
      tmp <- .data %>%
        mutate(yvar_clean = imputation_switcher(
          yvar = {{ yvar }},
          method = method,
          frequency = frequency,
          na_marker = na_marker_int
        ))
    } else if (length(method) == 1 & method != "winsorize") {
      tmp <- .data %>%
        mutate(
          na_marker_int = na_marker_int,
          yvar_clean = ifelse(na_marker_int == 1, NA, {{yvar}}) %>%
            imputation_switcher(
              method = method,
              frequency = frequency
            )
        ) %>%
        select(-na_marker_int)
    }
  } else if (is.null(na_exclude) == TRUE & is.null(method)==TRUE) { # Winsorize imputation if no regressors
    message(paste0("Even though you've selected: {", method, "}. There are no NA's to replace, winsorize has been applied instead."))
    tmp <- .data %>%
      mutate(yvar_clean = imputation_switcher(yvar = pull(select(.data, {{yvar}}))
                                              , method = "winsorize"
                                              , frequency = frequency)
             )
  }
  
  if (replace == TRUE) {
    tmp %>%
      mutate(!!quo({{yvar}}) := yvar_clean) %>% # masking names
      select(-yvar_clean)
  } else {
    tmp
  }
}



na_marker_int <- function(.data, na_exclude){
  .data %>% # na marker is formed as columns different from 0, thus, no reg_value
  select(-{{ na_exclude }}) %>% # exclude rule to the data.frame
  rowSums() != 0
  }


rowSums(.data[setdiff(names(.data), na_exclude)])!=0


na_marker_int(demo_1, na_exclude = na_exclude)

na_exclude = c("forecast_item", "date", "volume")


impute_ts(.data = demo_1, yvar = volume)


imputation_switcher(yvar = pull(select(.data, volume)), method = "winsorize")

# Cleansing ---------------------------------------------------------------

#' Time Series Cleansing
#' 
#' This function collects the data and applies imputation, time series cuts and leading zeros filtering
#'
#' @param .data DataFrame or tibble.
#' @param yvar Quoted or unquoted variable name to be cleansed.
#' @param method Method to be applied to the time series. Options: kalman, winsorize, 
#' mean, median, nearest observation, interpolation. If more than one method is chosen, a set of new columns
#' is automatically generated.
#' @param na_exclude Vector of names that **should not** be used to replace the observation by NA.
#' @param frequency Time series frequency.
#' @param replace Logical. If replace = TRUE (default) it will 
#' replace the previous yvar by its cleansed/imputated version. Otherwise, a "y_clean" column will
#' be attached to the data matrix.
#'
#' @return
#' @export
#'
#' @examples
cleansing <- function(.data, yvar, method, frequency = 12, na_exclude = NULL, replace = T, ...){
  .data %>% 
    filter(cumsum({{yvar}})>0) %>% # Leading zeros
    impute_ts(yvar = {{yvar}}, method = method, na_exclude = na_exclude, frequency = frequency, replace = replace, ...) %>% 
    rename(yvar = {{yvar}})
}




