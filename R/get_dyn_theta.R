#' Fit Theta Model
#'
#' @param .data Data frame or tibble.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param horizon Numeric. Number of periods to forecast.
#' 
#' @import forecast
#' @import stats
#' @import forecTheta
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_ets()
#' }
get_dyn_theta <- function(.data, y_var, horizon = 12, parameter = NULL){
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
  model_fit <- dotm(y_var_int, h = horizon)
  # Timelapse
  .fit_output <- list(model = "theta"
                      , model_fit = model_fit
                      , y_var_int = y_var_int
                      , y_var_fcst = as.numeric(model_fit$mean)
                      , parameter = model_fit$method
  )
  # Output
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
  options(warn = 1)
}
