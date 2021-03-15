#' Fit Croston model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param horizon Numeric. Number of periods to forecast.
#' @param parameter List. Parameter to be used for estimation, looks for alpha parameter.
#'
#' @import forecast
#' @import stats
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_croston()
#' }
fit_croston <- function(.data, parameter = NULL){
  horizon <- 1
  # Ts object
  y_var_int <- ts(.data[["y_var"]], frequency = .log_init$prescription$freq)
  # Fit
  if(is.null(parameter)){
    model_fit <- round(as.numeric(croston(y_var_int, h = horizon)[1]), 2)
  }else{
    model_fit <- round(as.numeric(croston(y_var_int
                                          , alpha = parameter[["croston"]][["alpha"]]
                                          , h = horizon)[1]), 2)
  }
  # Output
  class(model_fit) <- "num_fit"
  model_fit
}
