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
get_design_matrix <- function(.data, date_var=NULL, freq=NULL, parameter = NULL, to_dummy = TRUE){
  
  if(is.null(attributes(.data)[["prescription"]])==FALSE){
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
  }
  
  if(freq == 12){
    reg_seasonal <- function(date) factor(months(as.Date(date), abbreviate = T), levels = month.abb)
  } else if(freq == 4){
    reg_seasonal <- function(date) factor(as.factor(quarters(as.Date(date), abbreviate = T)), levels = paste0("Q", 1:4))
  } else if(round(freq, 0) == 52){
    reg_seasonal <- function(date) factor(lubridate::week(date), levels = 1:53)
  }
  
  seasonal_var <- reg_seasonal(.data[["date_var"]])
  trend <- 1:length(seasonal_var)
  
  .design_matrix <- .data %>% 
    bind_cols(tibble(trend = trend, seasonal_var = seasonal_var)) %>% 
    relocate("trend", "seasonal_var", .after = "y_var")
  
  attr(.design_matrix, "prescription") <- attributes(.data)[["prescription"]]
  return(.design_matrix)
}


#' Automatic Time Series Feature Engineering
#' 
#' This function applies different heuristics to add time series features to the original data.
#' 
#' @param .data data-frame or tibble
#'
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' feature_engineering_ts()
#' }
feature_engineering_ts <- function(.data){
  prescription <- attributes(.data)[["prescription"]]
  
  # Internal
  wide_reg_int <- function(.data){
    n_regressors <- n_distinct(.data[["reg_name"]])
    if(n_regressors == 1){
      .data_tmp <- .data %>% 
        select(-reg_value, -reg_name) %>% 
        get_design_matrix(to_dummy = FALSE)
    } else if(n_regressors> 1){
      .data_tmp <- .data %>% 
        pivot_wider(names_from = "reg_name", values_from = "reg_value") %>% 
        select(-matches("0|NA$")) %>% 
        janitor::clean_names() %>% 
        mutate_at(.vars = vars(-matches("date_var|key|y_var")), .funs = ~ifelse(is.na(.x), 0, .x))
      attr(.data_tmp, "prescription") <- prescription
      .data_tmp <- get_design_matrix(.data_tmp, to_dummy = FALSE)
    }
    attr(.data_tmp, "prescription") <- attributes(.data)[["prescription"]]
    return(.data_tmp)
  }
  wide_reg_int(.data)
}













