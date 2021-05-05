#' Fit Simple Time Series Linear Model
#'
#' @param .data Data frame or tibble.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param parameter List.
#' 
#' @import forecast
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_ets()
#' }
get_tslm <- function(.data, y_var, parameter = NULL){
  
  # Prescription
  options(warn = -1)
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  # Ts Object
  y_var_int <- ts(.data[[y_var]], frequency = freq)
  
  # Model fit
  model_fit <- tslm(y_var_int ~ trend + season)
  
  # Timelapse
  .fit_output <- list(model = "tslm"
                      , model_fit = model_fit
                      , y_var_int = y_var_int
                      , y_var_pred = model_fit$fitted.values
                      , parameter = model_fit$method
  )
  
  # Output
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
  options(warn = 1)
  
}
