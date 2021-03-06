#' Fit a Seasonal Naive Model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param parameter List. Optional parameters.
#' @param horizon Numeric. Number of periods ahead to forecast.
#' 
#' @import stats
#' @import forecast
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_seasonal_naive()
#' }
get_seasonal_naive <- function(.data, y_var, parameter = NULL, horizon = 30){
  # horizon = 30 to reuse forecast as subset in get_forecast
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var_int <- ts(.data[[y_var]], frequency = freq) # maybe not optimal
  model_fit <- snaive(y_var_int, h = horizon)
  
  .fit_output <- list(model = "seasonal_naive"
                      , model_fit = model_fit
                      , y_var_int = y_var_int
                      , y_var_pred = as.numeric(model_fit$fitted)
                      )
  
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
}
