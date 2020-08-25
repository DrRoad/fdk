
get_neural_network <- function(.data, y_var, x_data = NULL, parameter = NULL){
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var_int <- ts(.data[[y_var]], frequency = freq) # maybe not optimal
  
  model_fit <- nnetar(y = y_var_int)
  
  # Output
  .fit_output <- list(model = "neural_network"
                      , model_fit = model_fit
                      , y_var_pred = as.numeric(model_fit$fitted)
                      , parameter = as.numeric(unlist(str_extract_all(model_fit$method, "[0-9]{1,2}")
                                                      )
                                               )
                      )
  
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  
  return(.fit_output)
}