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
get_ets <- function(.data, y_var, parameter = NULL){
  options(warn = -1)
      if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
        prescription <- attributes(.data)[["prescription"]]
        y_var <- prescription$y_var
        date_var <- prescription$date_var
        freq <- prescription$freq
        na_exclude <- unique(c(prescription$key, y_var, date_var))
      }
      y_var_int <- ts(.data[[y_var]], frequency = freq)
      if(is.null(parameter) == TRUE){
        #message("ETS optimization...")
        model_fit <- ets(y = y_var_int, model = "ZZZ", damped = TRUE, allow.multiplicative.trend = FALSE)
      } else {
        model_fit <- ets(y = y_var_int, model = parameter[["ets"]][["ets"]]
                         , damped = TRUE, allow.multiplicative.trend = FALSE)
      }
      # Timelapse
      .fit_output <- list(model = "ets"
                          , model_fit = model_fit
                          , y_var_pred = as.numeric(model_fit[["fitted"]])
                          , parameter = paste0(model_fit$components[1:3], collapse = "")
      )
      attr(.fit_output, "prescription") <- prescription
      class(.fit_output) <- ".fit_output"
      return(.fit_output)
  options(warn = 1)
}
