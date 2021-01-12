#' Fill time series data
#'
#' @param .data 
#'
#' @return tibble
#' @export 
#'
fill_ts <- function(.data, na_value = 0){
  prescription <- attributes(.data)[["prescription"]]
  date_transf <- function(date_var, freq){
    freq <- as.numeric(freq)
    if(freq == 1){
      lubridate::year(date_var)
    } else if(freq == 12){
      yearmonth(date_var)
    } else if(freq == 4){
      yearquarter(date_var)
    } else if(freq == 52){
      yearweek(date_var)
    } else if(freq == 365){
      ymd(date_var)
    }
  }
  if(max(.data[["date_var"]]) < prescription$max_date){
    cat("Input data has a max time index lower than global, padding...\n")
    tmp <- .data %>% 
      dplyr::bind_rows(tibble(date_var = prescription$max_date)) %>% 
      dplyr::mutate(date_var = date_transf(date_var, freq = prescription[["freq"]])) %>% 
      tsibble::as_tsibble(index = date_var) %>% 
      tidyr::fill(key, .direction = "down") %>% 
      tidyr::replace_na(list("y_var" = 0, "reg_name" = 0, "reg_value" = 0))
  } else {
    tmp <- .data %>% 
      dplyr::mutate(date_var = date_transf(date_var, freq = prescription[["freq"]])) %>% 
      tsibble::as_tsibble(index = date_var) 
  }
  if(pull(has_gaps(tmp))==TRUE){
    cat("Input data has time index gaps, padding...\n")
    tmp <- tmp %>%
      fill_gaps(reg_name = 0, reg_value = 0, y_var = na_value) %>% 
      as_tibble() %>%
      mutate(date_var = as.Date(date_var)) %>% 
      fill(key, .direction = "down") %>% 
      replace_na(replace = list(y_var = na_value, reg_name = "0", reg_value = 0)) %>% 
      as_tibble() %>% 
      mutate(date_var = as.Date(date_var))
  } else {
    tmp <- tmp %>% 
      as_tibble() %>% 
      mutate(date_var = as.Date(date_var))
  }
  attr(tmp, "prescription") <- prescription
  return(tmp)
}

#' Time series descriptors
#'
#' @param .data Input tibble
#'
#' @return tibble
#' @export
#'
describe_ts <- function(.data){
  prescription <- attributes(.data)[["prescription"]]
  prescription[["size"]] <- nrow(.data)
  prescription[["intermittency"]] <- round(sum(.data[["y_var"]]==0)/length(.data[["y_var"]]), 2)
  prescription[["tail_zero"]] <- sum(cumsum(rev(.data$y_var))==0)
  attr(.data, "prescription") <- prescription
  return(.data)
}

#' Validate Time Series Data
#' 
#' This functions fill gaps in time series and provide revelant statistics
#'
#' @param .data Tibble
#'
#' @return Tibble
#' @export
#'
validate_ts <- function(.data, na_value = 0){
  .data %>% 
    fill_ts(na_value = na_value) %>% 
    describe_ts() 
}

#---
