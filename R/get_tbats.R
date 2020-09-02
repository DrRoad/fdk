#' Fit a TBATS model
#'
#' Trigonometric seasonality, Box-Cox transformation, ARMA errors, Trend and Seasonal (TBATS)
#' 
#' @param .data data-frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param parameter List. Optional parameters.
#'
#' @import forecast
#' @import stats
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_tbats()
#' }
get_tbats <- function(.data, y_var, parameter = NULL){
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var_int <- ts(.data[[y_var]], frequency = freq) # maybe not optimal
  
  model_fit <- tbats(y_var_int)
  
  .fit_output <- list(model = "tbats"
                      , model_fit = model_fit
                      , y_var_pred = as.numeric(model_fit[["fitted.values"]])
                      , parameter = unlist(model_fit$parameters$vect)
  )
  
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
}





