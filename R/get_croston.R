#' Fit Croston model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param horizon Numeric. Number of periods to forecast.
#' @param parameter List. Parameter to be used for estimation, looks for alpha parameter.
#'
#' @return
#' @export
#'
#' @examples
get_croston_exp <- function(.data, y_var, horizon = 10, parameter = NULL){
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var_int <- ts(.data[[y_var]], frequency = freq)
  
  if(is.null(parameter)){
      model_fit <- croston(y_var_int, h = horizon)
    }else{
      model_fit <- croston(y_var_int, alpha = parameter[["croston"]][["alpha"]], h = horizon)
    }
    .fit_output <- list(model = "croston"
                   , y_var_fcst = as.numeric(model_fit$mean)
                   , parameter = list(alpha = ifelse(is.null(parameter$croston$alpha), 0.1 , parameter$croston$alpha))
                         )
    
    attr(.fit_output, "prescription") <- prescription
    class(.fit_output) <- ".fit_output"
    return(.fit_output) # Output
}
