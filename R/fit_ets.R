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
  globalVariables(c(".log"))
  options(warn = -1)
  y_var_int <- ts(.data[["y_var"]], frequency = .log$prescription$freq)
  
  ets_out <- tryCatch(
    {
      ets(y = y_var_int, model = "ZZZ", damped = TRUE, allow.multiplicative.trend = FALSE)
    }
    , warning = function(warn){
      ets(y = y_var_int, model = "ZZZ", damped = FALSE, allow.multiplicative.trend = FALSE)
    }
  )
  return(ets_out)
}
