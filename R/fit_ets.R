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
fit_ets <- function(.data, parameter = NULL){
  key <- attributes(.data)[["key"]]
  .log[[key]]$dates_check$date_range[[2]]
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
  ets_out %>% 
    structure(.log = list(key = key)
              , fdk_class = "fit")
}
