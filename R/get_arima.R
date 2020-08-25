#' Fit Auto Regressive Integrated Moving Average model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param is_seasonal Logical. Defines if the optimization takes into account seasonal parameters.
#' @param parameter List. Combination of parameter to estimate the model.
#' @param freq Numeric. Time series frequency.
#'
#' @return
#' @export
#'
#' @examples
get_arima_exp <- function(.data, y_var, is_seasonal = TRUE, parameter = NULL, freq){
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var_int <- ts(.data[[y_var]], frequency = freq) # maybe not optimal
  
  if(is.null(parameter)==TRUE){
    message("ARIMA optimization...")
    model_fit <- auto.arima(y_var_int, stepwise = TRUE, seasonal = is_seasonal)
  } else if(is.null(parameter[["arima"]][["seasonal"]])==FALSE){
    model_fit <- Arima(y_var_int, order = as.numeric(parameter[["arima"]][c("p", "d", "q")])
                       , seasonal = c(as.numeric(parameter[["arima"]][c("P", "D", "Q")], freq)))
  } else {
    model_fit <- Arima(y_var_int, order = as.numeric(parameter[["arima"]][c("p", "d", "q")]))
  }
  
  .fit_output <- list(model = "arima"
                 , model_fit = model_fit
                 , fit_summary = list(
                   data_size = length(.data[,1][[1]])
                   , y_var_pred = as.numeric(model_fit$fitted)
                 )
                 , parameter = as.list(arimaorder(model_fit)))
  
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
}




  
