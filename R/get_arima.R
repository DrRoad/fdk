

# ARIMA -------------------------------------------------------------------

get_arima_experimental <- function(.data, y_var, is_seasonal = TRUE, parameter = NULL, freq = 12){
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    frequency <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var_int <- ts(.data[[y_var]], frequency = freq) # maybe not optimal
  
  if(is.null(parameter)==TRUE){
    message("ARIMA optimization...")
    model_fit <- auto.arima(y_var_int, stepwise = TRUE, seasonal = is_seasonal)
  } else if(is.null(parameter[["arima"]][["seasonal"]])==FALSE){
    model_fit <- Arima(y_var_int, order = parameter[["arima"]][["order"]], seasonal = parameter[["arima"]][["seasonal"]])
  } else {
    model_fit <- Arima(y_var_int, order = parameter[["arima"]][["order"]])
  }
  
  .fit_output <- list(model = "arima"
                 , model_fit = model_fit
                 , parameter = arimaorder(model_fit))
  
  attr(.fit_output, "prescription") <- prescription
  
  return(.fit_output)
}




  
