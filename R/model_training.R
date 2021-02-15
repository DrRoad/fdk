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
  
  if(is.null(parameter)){
    grid_glmnet <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.025)
                               , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1)
                               , alpha = seq(from = 0, to = 1, by = 0.25))
    grid_glm <- expand_grid(time_weight = seq(from = 0.8, to = 1, by = 0.025)
                            , trend_discount = c(0.7,0.8,0.9,0.95,0.99,1))
    parameter <- list(glmnet = list(time_weight = .95, trend_discount = .70, alpha = 0, lambda = .1
                                    , grid_glmnet = grid_glmnet
                                    , job = list(optim_lambda = TRUE, x_excluded = NULL
                                                 , random_search_size = 0.3
                                                 , n_best_model = 1))
                      , croston = list(alpha = 0.1)
                      , glm = list(time_weight = .99, trend_discount = 0.70
                                   , grid_glm = grid_glm
                                   , job = list(x_excluded = NULL
                                                , random_search_size = 0.3
                                                , n_best_model = 1))
                      , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                      , ets = list(ets = "ZZZ"))
  }
  
  # Model selection
  if(ts_model == "glmnet"){
    fit_glmnet(.data = .data, parameter = parameter)
  } else if(ts_model == "glm"){
    get_glm(.data = .data, parameter = parameter)
  } else if(ts_model == "ets"){
    get_ets(.data = .data, parameter = parameter)
  } else if(ts_model == "arima"){
    get_arima(.data = .data, parameter = parameter)
  } else if(ts_model == "neural_network"){
    get_neural_network(.data = .data, parameter = parameter)
  } else if(ts_model == "seasonal_naive"){
    get_seasonal_naive(.data = .data, parameter = parameter)
  } else if(ts_model == "tbats"){
    get_tbats(.data = .data, parameter = parameter)
  } else if(ts_model == "croston"){
    get_croston(.data = .data, parameter = parameter)
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
