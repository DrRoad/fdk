#' Generate trend and seasonal components for regression based models
#'
#' @param .data data-frame or tibble
#' @param freq Numeric. Time series frequency
#' @param numeric_seas logical: whether or not to print numberic seasonal variables instead of factor.
#' @param hierarchy_seas logical: whether or not to provide higher aggregation levels. For instance, 
#' a time series of weekly frequency will generate besides week_seas, also month_seas, and year_seas.
#'
#' @return data-frame or tibble
#' @importFrom purrr map
#' @importFrom purrr map2
#' @import tidyverse
#' @import lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' get_design_matrix()
#' }
seasonal_features <- function(.data, freq = .log$prescription$freq
                              , numeric_seas = FALSE
                              , hierarchy_seas = FALSE){

  .data_tmp <- .data
  
  get_year_seas <- function(date) factor(lubridate::year(as.Date(date)))
  get_quarter_seas <- function(date) factor(as.factor(quarters(as.Date(date), abbreviate = T)), levels = paste0("Q", 1:4))
  get_month_seas <- function(date) factor(months(as.Date(date), abbreviate = T), levels = month.abb)
  get_week_seas <- function(date) factor(lubridate::week(date), levels = 1:53)
  get_day_seas <- function(date) factor(weekdays(date, abbreviate = T), levels = c("Sun", "Mon", "Tue", "Wed"
                                                                               , "Thu", "Fri", "Sat"))
  
  seas_list <- list(day_seas = get_day_seas, week_seas = get_week_seas
                    , month_seas = get_month_seas, quarter_seas = get_quarter_seas
                    , year_seas = get_year_seas)
  
  get_seas <- function(.data, numeric_seas, hierarchy_seas){
    if(hierarchy_seas == TRUE){
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
                           , hierarchy_seas = hierarchy_seas)

  # Log ---------------------------------------------------------------------

  log_update(module = "seasonal_features"
             , key = attributes(.data)[["key"]]
             , new_log = list(numeric_seas = numeric_seas
                              , hierarchy_seas = hierarchy_seas
                              , seasonal_features = names(seasonal_var))
             )
  
  # Return ------------------------------------------------------------------

  .data_tmp %>% 
    mutate(trend = 1:n()) %>% 
    bind_cols(seasonal_var)
}


#' Long to wide regressor column
#'
#' @param .data tibble/data.frame
#'
#' @return
#' @export
#'
#' @examples
long_to_wide <- function(.data){
  
  key <- attributes(.data)[["key"]]
  reg_names <- .data$reg_name %>% 
    enframe() %>% 
    mutate(is_valid = case_when(
      value == "" ~ FALSE
      , is.na(value) ~ FALSE
      , T ~ T
    )) %>% 
    filter(is_valid == T) %>% 
    na.omit() %>% 
    pull(value) %>% 
    unique()
  
  #n_regressors <- n_distinct(setdiff(tmp[["reg_name"]], c("0", "NA", NA_character_)))
  
  if(length(.log[[key]]$dates_check$dates_with_reg) == 0 & length(reg_names) == 0){

    .data <- .data %>% 
      dplyr::select(-all_of(c("reg_value", "reg_name")))
    
    log_update(module = "long2wide"
               , key = key
               , new_log = list(warning = "No regressors' found."))
  } else {
    .data <- .data %>% 
      pivot_wider(names_from = "reg_name", values_from = "reg_value") %>%
      dplyr::select(-matches("^0|^NA")) %>% 
      janitor::clean_names() %>% 
      dplyr::mutate_at(.vars = vars(-matches("date_var|key|y_var"))
                       , .funs = ~ifelse(is.na(.x), 0, .x))
    
    # regressor names

    log_update(module = "long2wide"
               , key = key
               , new_log = list(
                 regressor_names = setdiff(names(.data)
                                           , c("key", "date_var", "y_var"))
               ))
  }
  
  return(structure(.data, key = key))
}


#' Autoregressive or lags features
#' 
#' This function generate AR for response and features
#'
#' @param .data data-frame or tibble
#' @param lag_var list: defines the number of lag periods for a given variable, for instance, 
#' list(y_var = 1) will generate a lag (autoregressive) variable of 1 period.
#'
#' @importFrom purrr map
#' @importFrom purrr map2
#' @import tidyverse
#' @importFrom tidyr unnest
#' @return
#' @export
#'
#' @examples
lag_features <- function(.data, lag_var = list()){
  
  .data_tmp <- .data

  stopifnot(is.list(lag_var) | is.null(lag_var))
  #stopifnot(length(lag_var) == length(n_lag))
  suppressMessages(
    if(is.null(lag_var) | length(lag_var) == 0){
      .data_tmp
    } else {
      stopifnot(all(names(lag_var) %in% names(.data_tmp)))
      grid_lag <- unnest(enframe(lag_var, name = "lag_var", value = "n_lag"), cols = "n_lag")
      
      .data_tmp <- map2(grid_lag$lag_var, grid_lag$n_lag, ~dplyr::lag(.data_tmp[[.x]], n = .y)) %>% 
        setNames(paste0(grid_lag$lag_var,"_lag_",  grid_lag$n_lag)) %>% 
        bind_cols(.data_tmp, .)
      
      log_update(module = "lag_features"
                 , key = attributes(.data)[["key"]]
                 , new_log = list(lag_var = lag_var))

      return(.data_tmp)
    }
  )
}

#' Moving Average feature engineering
#'
#' @param .data tibble
#' @param ma_var list: list of strings defining the desired configuration. 
#'
#' @importFrom purrr map_dbl
#' @return
#' @export
#'
#' @examples
ma_features <- function(.data, ma_var = list()){
  
  .data_tmp <- .data
  
  stopifnot(is.list(ma_var) | is.null(ma_var))
  
  if(length(ma_var)!=0){
    if(any(map_dbl(names(ma_var), ~length(ma_var[[.x]])) == 1) == T){
      stop("Moving average needs to specify the number of periods before and after explicitly")
    }
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
    
    log_update(module = "ma_features"
               , key = attributes(.data)[["key"]]
               , new_log = list(ma_var = ma_var))
    
    .data_tmp
  } else {
    .data_tmp
  }
}


#' Automatic Time Series Feature Engineering
#' 
#' This function applies different heuristics to add time series features to the original data.
#' 
#' @param .data data-frame or tibble
#' @param lag_var list: defines the number of lag periods for a given variable, for instance, 
#' list(y_var = 1) will generate a lag (autoregressive) variable of 1 period.
#' @param ma_var list: defines the number of periods before and after to apply a mean, for instance, 
#' list(y_var = c(1, 2)) will generate a moving average of 1 period before and 2 after a given point.
#' @param numeric_seas logical: whether or not to print numberic seasonal variables instead of factor.
#' @param hierarchy_seas logical: whether or not to provide higher aggregation levels. For instance, 
#' a time series of weekly frequency will generate besides week_seas, also month_seas, and year_seas.
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' feature_engineering_ts()
#' }
feature_engineering_ts <- function(.data, lag_var = list(), ma_var = list()
                                   , numeric_seas = FALSE, hierarchy_seas = FALSE){
  # Export

  .data %>% 
    long_to_wide() %>% 
    seasonal_features(numeric_seas = numeric_seas, hierarchy_seas = hierarchy_seas) %>% 
    lag_features(lag_var = lag_var) %>% 
    ma_features(ma_var = ma_var) %>% 
    na.omit() %>% 
    mutate(trend = 1:n())
}

