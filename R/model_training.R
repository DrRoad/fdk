#' Fitting models for time-series data
#' 
#' Wrapper of single function call to simplify the estimation
#'
#' @param .data data-frame or tibble
#' @param y_var Column name of the variable to be forecasted
#' @param date_var Column name of the time index
#' @param model String. Name of the model to be estimated.
#' @param parameter List. Optional set of parameters to estimate models.
#'
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' fit_ts()
#' }
fit_ts <- function(.data, ts_model, parameter = list()){

  # Params

  # Model selection
  if(ts_model == "glmnet"){
    fit_glmnet(.data = .data, parameter = parameter)
  } else if(ts_model == "glm"){
    fit_glm(.data = .data, parameter = parameter)
  } else if(ts_model == "ets"){
    fit_ets(.data = .data, parameter = parameter)
  } else if(ts_model == "arima"){
    fit_arima(.data = .data, parameter = parameter)
  } else if(ts_model == "neural_network"){
    get_neural_network(.data = .data, parameter = parameter)
  } else if(ts_model == "seasonal_naive"){
    get_seasonal_naive(.data = .data, parameter = parameter)
  } else if(ts_model == "tbats"){
    get_tbats(.data = .data, parameter = parameter)
  } else if(ts_model == "croston"){
    fit_croston(.data = .data, parameter = parameter)
  } else if(ts_model == "dyn_theta"){
    get_dyn_theta(.data = .data, parameter = parameter)
  } else if(ts_model == "prophet"){
    suppressWarnings({get_prophet(.data = .data_tmp, parameter = parameter)})
  } else if(ts_model == "tslm"){
    get_tslm(.data = .data, parameter = parameter)
  } else if(ts_model == "svm"){
    get_svm(.data = .data, parameter = parameter)
  } else if(ts_model == "gam"){
    fit_gam(.data = .data, parameter = parameter)
  }
}

#---
