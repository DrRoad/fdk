#' Fit SVM model
#'
#' @param .data Data frame or tibble.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param parameter List. Combination of parameter to estimate the model.
#' 
#' @import forecast
#' @import e1071
#' @import stats
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_ets()
#' }
get_svm <- function(.data, y_var, parameter = NULL){
  options(warn = -1)
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  # Data
  xd <- .data 
  # Model Fit
  Errorcheck <- try(tune(svm, y_var ~ trend + seasonal_var, data=xd), silent = TRUE)  
  # New data
  new.data <- data.frame(trend = xd$trend, seasonal_var = xd$seasonal_var)
  # Run with error check
  if (class(Errorcheck)== "try-error") { 
    f_svm = 0 } else {
      model_fit <- tune(svm, y_var ~ trend + seasonal_var, data=xd, 
                    type="eps-regression",
                    ranges=list(elsilon=seq(0,1,0.1),
                                gamma=c(1/12,1), 
                                cost=10^(-1:2)))$best.model
      predicted <- predict(x_svm, new.data)
  }
  # Timelapse
  .fit_output <- list(model = "svm"
                      , model_fit = model_fit
                      , y_var_pred = predicted
                      , parameter = paste0("Cost:",round(model_fit$cost,2),"; ","Gamma:",round(model_fit$gamma,2),"; ","Epsilon:",round(model_fit$epsilon,2))
  )
  attr(.fit_output, "prescription") <- prescription
  class(.fit_output) <- ".fit_output"
  return(.fit_output)
  options(warn = 1)
}
