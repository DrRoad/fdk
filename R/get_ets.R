
# ets

get_ets_exp <- function(.data, y_var, parameter, horizon){
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var_int <- ts(.data[[y_var]], frequency = freq)
  
  model_fit <- ets(y = y_var_int, model=parameter[["ets"]][["ets"]], damped = NULL, allow.multiplicative.trend = FALSE)
  
  # Timelapse
  .fit_output <- list(model = "ets"
                      , model_fit = model_fit
                      , y_var_pred = as.numeric(model_fit[["fitted"]])
                      , parameter = paste0(model_fit$components[1:3], collapse = "")
                      )
  
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
}
