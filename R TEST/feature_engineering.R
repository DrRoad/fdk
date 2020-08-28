#' Generate trend and seasonal components for regression based models
#'
#' @param .data DataFrame or tibble
#' @param date_var String. Column name of the time index variable
#' @param freq Numeric. Time series frequency
#' @param parameter List.
#' @param to_dummy Logical. Convert design matrix factors to binary.
#'
#' @return
#' @export
#'
#' @examples
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
  
  seasonal_var <- reg_seasonal(.data[[date_var]])
  trend <- 1:length(seasonal_var)
  
  .data %>% 
    bind_cols(tibble(trend = trend, seasonal_var = seasonal_var)) %>% 
    relocate("trend", "seasonal_var", .after = "y_var")
}


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
    return(.data_tmp)
  }
  cat("\nProcedures applied: \n- Feature engineering\n")
  wide_reg_int(.data)
}











l %>% 
  mutate(n_regressors = map_lgl(data, ~n_distinct(.x[["reg_name"]])>1)
         , data = map2(n_regressors, data, .f = ~{
           if(.x == TRUE){
             .y %>% 
               select(-reg_value, -reg_name)
           } else {
             wider_tmp <- .y %>% 
               pivot_wider(names_from = "reg_name", values_from = "reg_value") %>% 
               select(-matches("0|NA$")) %>% 
               janitor::clean_names() %>% 
               mutate_at(.vars = vars(-matches("date_var|key|y_var")), .funs = ~ifelse(is.na(.x), 0, .x))
             attr(wider_tmp, "prescription") <- attributes(.data)[["prescription"]]
             get_design_matrix(wider_tmp)
           }
         }))





map(l$data, ~wide_reg(.x))




wide_reg(.data) %>% attributes()














