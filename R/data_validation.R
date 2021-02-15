#' Validate Time Series Data
#' 
#' This functions fill gaps in time series and provide revelant statistics
#'
#' @param .data Tibble
#'
#' @return Tibble
#' @export
#'
validate_ts <- function(.data, na_values = list(y_var = 0, reg_value = 0, reg_name = "")){
  
  .data_tmp <- .data
  
  fill_ts <- function(.data){
    seq_complete <- seq.Date(from = as.Date(min(.data[["date_var"]]))
                             , to = as.Date(max(.data[["date_var"]]))
                             , by = as.character(.log_init$prescription$freq_name))
    n_missing_dates <- (length(seq_complete) - length(.data[["date_var"]]))
    missing_dates <- base::setdiff(as.character(seq_complete), as.character(.data[["date_var"]]))
    dates_with_reg <- .data %>% 
      filter(is.na(reg_name)==F | reg_name != "") %>% 
      pull(date_var) %>% 
      unique() %>% 
      as.character()
    
    duplicated_dates <- .data[["date_var"]][duplicated(.data[["date_var"]])]
    
    if(length(duplicated_dates)>0){
      suspicious_cases <- .data %>% 
        filter(date_var %in% duplicated_dates)
      
      if(n_distinct(suspicious_cases$reg_name) > length(duplicated_dates)){
        warning_log <- "Multiple regressors have been found for a same date"
        if(sum(suspicious_cases$reg_value == 0) == length(duplicated_dates)){
          solution_log <- "Zeros have been found, they will be filtered out"
          vec_excluded <- (.data$reg_value == 0 & .data$date_var %in% duplicated_dates)
          .data <- .data[!vec_excluded, ]
        } else {
          solution_log <- "No solution has been applied"
        }
      }
      
      log_update(module = "dates_check"
                 , key = attributes(.data)[["key"]]
                 , new_log = list(n_dates = length(seq_complete)
                                  , duplicated_dates = duplicated_dates
                                  , missing_dates = missing_dates
                                  , dates_with_reg = dates_with_reg
                                  , warning = warning_log
                                  , solution = solution_log))
    } else {
      log_update(module = "dates_check"
                 , key = attributes(.data)[["key"]]
                 , new_log = list(n_dates = length(seq_complete)
                                  , warning = "No duplicated dates have been found."))
    }
    
    # Output ------------------------------------------------------------------
    
    tmp <- seq_complete %>% 
      enframe(value = "date_var") %>% 
      left_join(.data, by = "date_var") %>%
      do({
        tmp_1 <- tibble(.)
        if(all(is.na(tmp_1$reg_name))){
          tmp_1 %>% 
            replace_na(replace = list(y_var = na_values[["y_var"]]
                                      , reg_value = na_values[["reg_value"]]
                                      , reg_name = na_values[["reg_name"]]))
        } else {
          tmp_1 %>% 
            #fill("reg_name", .direction = "down") %>% 
            replace_na(replace = list(reg_value = 0
                                      , y_var = na_values[["y_var"]]))
        }
      }) %>% 
      dplyr::select(-name) %>% 
      structure("key" = attributes(.data)[["key"]])
    
    return(tmp)
  }
  
  .data %>% 
    fill_ts()
  
  #list(.data = fill_ts(.data), logger)
}
