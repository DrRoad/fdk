#' Generate trend and seasonal components for regression based models
#'
#' @param .data data-frame or tibble
#' @param date_var String. Column name of the time index variable
#' @param freq Numeric. Time series frequency
#' @param parameter List.
#' @param to_dummy Logical. Convert design matrix factors to binary.
#'
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_design_matrix()
#' }
seasonal_features <- function(.data, freq = af_log$log[[1]]$freq, numeric_seas = FALSE, level_seas = T){
  
  .log_tmp <- .data$.log
  .data_tmp <- .data$.data
  
  get_year_seas <- function(date) factor(lubridate::year(as.Date(date)))
  get_quarter_seas <- function(date) factor(as.factor(quarters(as.Date(date), abbreviate = T)), levels = paste0("Q", 1:4))
  get_month_seas <- function(date) factor(months(as.Date(date), abbreviate = T), levels = month.abb)
  get_week_seas <- function(date) factor(lubridate::week(date), levels = 1:53)
  get_day_seas <- function(date) factor(weekdays(date, abbreviate = T), levels = c("Sun", "Mon", "Tue", "Wed"
                                                                               , "Thu", "Fri", "Sat"))
  
  seas_list <- list(day_seas = get_day_seas, week_seas = get_week_seas
                    , month_seas = get_month_seas, quarter_seas = get_quarter_seas
                    , year_seas = get_year_seas)
  
  get_seas <- function(.data, numeric_seas, level_seas){
    if(level_seas == TRUE){
      if(freq == 365){
        .seas_tmp <- map(.x = seas_list[5:1], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 52){
        .seas_tmp <- map(.x = seas_list[5:2], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 12){
        .seas_tmp <- map(.x = seas_list[5:3], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 4){
        .seas_tmp <- map(.x = seas_list[5:4], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 1){
        .seas_tmp <- map(.x = seas_list[5:5], ~.x(.data$date_var)) %>%
          bind_cols()
      }
    } else {
      if(freq == 365){
        .seas_tmp <- map(.x = seas_list[1], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 52){
        .seas_tmp <- map(.x = seas_list[2], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 12){
        .seas_tmp <- map(.x = seas_list[3], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 4){
        .seas_tmp <- map(.x = seas_list[4], ~.x(.data$date_var)) %>%
          bind_cols()
      } else if(freq == 1){
        .seas_tmp <- map(.x = seas_list[5], ~.x(.data$date_var)) %>%
          bind_cols()
      }
    }
    
    if(numeric_seas == T){
      .seas_tmp %>% 
        mutate(across(.cols = 1:last_col(), .fns = ~as.numeric(.x)))
    } else {
      .seas_tmp
    }
  }

  seasonal_var <- get_seas(.data = .data_tmp
                           , numeric_seas = numeric_seas
                           , level_seas = level_seas)
  .data_tmp <- .data_tmp %>% 
    mutate(trend = 1:n()) %>% 
    bind_cols(seasonal_var)
  
  list(.data = .data_tmp, .log = .log_tmp)
}


#' Long to wide regressor column
#'
#' @param .data 
#'
#' @return
#' @export
#'
#' @examples
long_to_wide <- function(.data){
  
  .log_tmp <- .data$.log
  .data_tmp <- .data$.data
  
  #n_regressors <- n_distinct(setdiff(tmp[["reg_name"]], c("0", "NA", NA_character_)))
  
  if(.log_tmp$log[[1]]$has_reg == FALSE){
    .data_tmp %>% 
      dplyr::select(-reg_value, -reg_name)
    
  } else {
    .data_tmp_wide <- .data_tmp %>% 
      pivot_wider(names_from = "reg_name", values_from = "reg_value") %>%
      dplyr::select(-matches("^0|^NA")) %>% 
      janitor::clean_names() %>% 
      dplyr::mutate_at(.vars = vars(-matches("date_var|key|y_var")), .funs = ~ifelse(is.na(.x), 0, .x))
    
    # regressor names
    
    reg_names <- setdiff(names(.data_tmp_wide), c("key", "date_var", "y_var"))
    
    wide_reg_log <- update_logger(key = attributes(.data_tmp)[["key"]]
                                  , module = "long_to_wide_format"
                                  , new_log = tibble(regressor_names = list(reg_names)))
    
    list(.data = .data_tmp_wide, .log = bind_rows(.log_tmp, wide_reg_log))
  }
}


#' Autoregressive or lags features
#' 
#' This function generate AR for response and features
#'
#' @param .data 
#' @param lag_var 
#' @param n_lag 
#'
#' @return
#' @export
#'
#' @examples
lag_features <- function(.data, lag_var = list()){
  .log_tmp <- .data$.log
  .data_tmp <- .data$.data

  stopifnot(is.list(lag_var) | is.null(lag_var))
  #stopifnot(length(lag_var) == length(n_lag))
  suppressMessages(
    if(is.null(lag_var) | length(lag_var) == 0){
      .data
    } else {
      stopifnot(all(names(lag_var) %in% names(.data_tmp)))
      grid_lag <- unnest(enframe(lag_var, name = "lag_var", value = "n_lag"), cols = "n_lag")
      
      .data_tmp <- map2(grid_lag$lag_var, grid_lag$n_lag, ~dplyr::lag(.data_tmp[[.x]], n = .y)) %>% 
        setNames(paste0(grid_lag$lag_var,"_lag_",  grid_lag$n_lag)) %>% 
        bind_cols(.data_tmp, .)
      
      list(.data = .data_tmp, .log = .log_tmp)
    }
  )
}

ma_features <- function(.data, ma_var = list()){
  .log_tmp <- .data$.log
  .data_tmp <- .data$.data
  
  stopifnot(is.list(ma_var) | is.null(ma_var))
  
  if(length(ma_var)!=0){
    grid_ma <- enframe(ma_var, name = "ma_var", value = "window_ma") %>% 
      rowwise() %>%
      mutate(names = paste0(unlist(window_ma), collapse = "_")
             , names = paste0(ma_var, "_ma_", names)) %>% 
      ungroup()
    
    suppressMessages(
      .data_tmp <- map2(.x = grid_ma$ma_var, .y = grid_ma$window_ma
                        , .f = ~slider::slide_dbl(.x = .data_tmp[[.x]]
                                                  , .f = mean
                                                  , .before = .y[[1]]
                                                  , .after = .y[[2]])) %>%
        setNames(nm = grid_ma$names) %>% 
        bind_cols(.data_tmp, .)
    )
    
    list(.data = .data_tmp, .log = .log_tmp)
  } else {
    .data
  }
}


#' Automatic Time Series Feature Engineering
#' 
#' This function applies different heuristics to add time series features to the original data.
#' 
#' @param .data data-frame or tibble
#' @param lag_var String. Name of the regressor or dependent variable (y_var) to be lagged.
#' @param n_lag Numeric. How many lags to create for each lag_var
#'
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' feature_engineering_ts()
#' }
feature_engineering_ts <- function(.data, lag_var = list(), ma_var = list(), numeric_seas = FALSE, level_seas = FALSE){
  # Export
  
  .data %>% 
    long_to_wide() %>% 
    seasonal_features(numeric_seas = numeric_seas, level_seas = level_seas) %>% 
    lag_features(lag_var = lag_var) %>% 
    ma_features(ma_var = ma_var)
}
