#' Fit Auto Regressive Integrated Moving Average Model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param parameter List. Combination of parameter to estimate the model.
#'
#' @import forecast
#' @importFrom utils globalVariables
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_arima()
#' }
fit_arima <- function(.data, parameter = NULL){
  key <- attributes(.data)[["key"]]
  .log[[key]]$dates_check$date_range[[2]]
  freq <- .log$prescription$freq  
  y_var_int <- ts(.data[["y_var"]], frequency = freq) # maybe not optimal
  
  arima_fit <- tryCatch(
    {
      if(parameter[["arima"]]$auto_arima == TRUE){
        message("ARIMA optimization...")
        auto.arima(y_var_int, stepwise = TRUE
                                , seasonal = parameter[["arima"]][["search_seasonal"]])
      } else if(any(is.na(parameter[["arima"]]$pdq[4:6])) == FALSE){
        suppressWarnings(Arima(y_var_int
                                            , order = parameter[["arima"]]$pdq[1:3]
                                            , seasonal = parameter[["arima"]][["pdq"]][4:6]
                                            #, method = "ML"
                               ))
      } else {
        suppressWarnings(Arima(y_var_int
                                            , order = parameter[["arima"]]$pdq[1:3]
                                            #, method = "ML"
                               ))
      }
    }
    , error = function(err){
      suppressWarnings(Arima(y_var_int
                             , order = c(0, 0, 0)
                             #, method = "ML"
                             ))
    }
  )
  arima_fit %>% 
    structure(.log = list(key = key))
}
