#' Fit Error Trend Seasonal model
#'
#' @param .data Data frame or tibble.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param parameter List. Combination of parameter to estimate the model.
#' 
#' @import forecast
#' @import stats
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_ets()
#' }
fit_ets <- function(.data, y_var, parameter = NULL){
  options(warn = -1)
  y_var_int <- ts(.data[["y_var"]], frequency = .log$prescription$freq)
  
  tryCatch(
    {
      model_fit <- ets(y = y_var_int, model = "ZZZ", damped = TRUE, allow.multiplicative.trend = FALSE)
    }
    , warning = function(warn){
      model_fit <- ets(y = y_var_int, model = "ZZZ", damped = F, allow.multiplicative.trend = FALSE)
    }
  )
  model_fit
}
