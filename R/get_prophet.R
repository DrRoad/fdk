#' Fit Prophet Model
#'
#' @param .data Data frame or tibble.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param horizon Numeric. Number of periods to forecast.
#' @param parameter List.
#' 
#' @import forecast
#' @import stats
#' @import prophet
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_ets()
#' }
get_prophet <- function(.data, y_var, horizon = 12, parameter = NULL){
  # Prescription
  options(warn = -1)
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  # Freq handling
  weekly.seasonality <- daily.seasonality  <- yearly.seasonality <- FALSE
  if(freq == 12){
    yearly.seasonality <- "auto"
    frequency <- "months"
  } else if(freq == 52){
    weekly.seasonality <- TRUE
    frequency <- "weeks"
  } else if(freq == 365){
    daily.seasonality <- TRUE
    frequency <- "days"
  }
  # Ts object
  xd <- .data %>% rename(y = y_var, ds = date_var)
  # Model fit
  model_fit <- prophet(xd, 
                   daily.seasonality = daily.seasonality,
                   weekly.seasonality = weekly.seasonality,
                   yearly.seasonality = yearly.seasonality,
                   seasonality.prior.scale = 1,
                   seasonality.mode = "additive",
                   changepoint.prior.scale = 30,
                   n.changepoints = 1,
                   growth = "linear")
  # Future
  predicted <- predict(model_fit, xd)
  # Timelapse
  .fit_output <- list(model = "prophet"
                      , model_fit = model_fit
                      , y_var_int = xd$y
                      , y_var_fcst = as.numeric(predicted$yhat)
                      , parameter = "Prophet"
  )
  # Output
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
  options(warn = 1)
}
