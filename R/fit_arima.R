#' Fit Auto Regressive Integrated Moving Average Model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param is_seasonal Logical. Defines if the optimization takes into account seasonal parameters.
#' @param parameter List. Combination of parameter to estimate the model.
#' @param freq Numeric. Time series frequency.
#'
#' @import forecast
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_arima()
#' }
fit_arima <- function(.data, parameter = NULL){
  freq <- .log$prescription$freq
  y_var_int <- ts(.data[["y_var"]], frequency = freq) # maybe not optimal
  if(parameter[["arima"]]$auto_arima == TRUE){
    message("ARIMA optimization...")
    arima_fit <- auto.arima(y_var_int, stepwise = TRUE
                            , seasonal = parameter[["arima"]][["search_seasonal"]])
  } else if(any(is.na(parameter[["arima"]]$pdq[4:6])) == FALSE){
    arima_fit <- suppressWarnings(Arima(y_var_int
                                        , order = parameter[["arima"]]$pdq[1:3]
                                        , seasonal = parameter[["arima"]][["pdq"]][4:6]
                                        , method = "ML"))
  } else {
    arima_fit <- suppressWarnings(Arima(y_var_int
                                        , order = parameter[["arima"]]$pdq[1:3]
                                        , method = "ML"))
  }
  return(arima_fit)
}
