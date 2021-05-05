#' Fitting models for time-series data
#' 
#' Wrapper of single function call to simplify the estimation
#'
#' @param .data tibble/data.frame: matrix of response and covariates.
#' @param ts_model string: name of the mode to be fitted.
#' @param parameter list: Combination of parameter to estimate the model.
#'
#' @return data-frame
#' @import tidyverse
#' @import forecast
#' @import mgcv
#' @import glmnet
#' 
#' 
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
    suppressWarnings({get_prophet(.data = .data, parameter = parameter)})
  } else if(ts_model == "tslm"){
    get_tslm(.data = .data, parameter = parameter)
  } else if(ts_model == "svm"){
    get_svm(.data = .data, parameter = parameter)
  } else if(ts_model == "gam"){
    fit_gam(.data = .data, parameter = parameter)
  }
}

#' High order optimization function
#'
#' @param .data tibble
#' @param ts_model character
#' @param optim_conf list of optimization configuration
#' @param parameter list of parameters
#' @param export_fit logical
#'
#' @return
#' @import tidyverse
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr transpose
#' @import tidyr
#' @export
#'
#' @examples
optim_ts <- function(.data, ts_model = character()
                     , optim_conf = list()
                     , parameter = list()
                     , export_fit = FALSE){
  
  interm_rule <- (sum(.data$y_var==0)/nrow(.data)>.3)
  size_rule <- (nrow(.data) - optim_conf$test_size - optim_conf$lag)<13
  ts_models_rule <- any(ts_model %in% c("glmnet", "gam", "glm", "arima")) == F
  
  
  if((interm_rule | size_rule)==T){
    message(paste0("Too much intermittency or small sample to tune model <"
                   , toupper(paste0(ts_model, collapse = ", ")), ">."))
  } else {
    map(unlist(ts_model), function(ts_model_i){
      parameter_list <- get_hyperpar_sample(ts_model = ts_model_i
                                        , parameter = get_default_hyperpar()
                                        )
      best_hyperpar <- map(1:length(parameter_list), function(parameter_i){
        optim_int(.data = .data
                  , ts_model = ts_model_i
                  , optim_conf = optim_conf
                  , parameter = parameter_list[[parameter_i]]
                  , export_fit = export_fit) %>% 
          .[c("model","parameter", "mape", "spa", "mse", "mae")]}) %>% 
        transpose() %>% 
        enframe() %>% 
        pivot_wider() %>%
        mutate(across(.cols = -2, .fns = ~list(unlist(.x)))) %>% 
        unnest(c(mse, mae, spa, mape, model, parameter)) %>% 
        mutate(index = 1:n()
               , spa_d = abs(round(spa - 1, 2))
               , across(c("mape", "spa_d", "mse", "mae")
                        , .fns = list(rank = ~rank(.x
                                                   , ties.method = "first")
                        ))
               , rank_agg = mape_rank + mse_rank + mae_rank) %>% 
        dplyr::select(index, model, everything(), -matches("_rank"))}) %>% 
      bind_rows() %>% 
      arrange(mape)
  }
}

#' Time series optimization internal function
#'
#' @param .data tibble/data.frame
#' @param ts_model string: model to be optimized.
#' @param optim_conf list: optimization configuration.
#' @param parameter list: parameters and hyperparameters.
#' @param export_fit logical: whether or not to include fit object.
#' 
#' @import tidyverse
#' @import utils
#' @importFrom purrr map
#' @importFrom purrr map2
#'
#' @return
#' @export
#'
#' @examples
optim_int <- function(.data
                      , ts_model = character()
                      , optim_conf = list()
                      , parameter = list()
                      , export_fit = FALSE){
  
  # What to do if the conditions are not met
  
  train_index_int <- 1:(nrow(.data) - (optim_conf$test_size + optim_conf$lag))
  test_index_int <- (nrow(.data) - optim_conf$test_size + 1):nrow(.data)
  
  #iter_count <- 0
  optim_out <- map(1:optim_conf$test_size, function(iter){
    #iter_count <<- iter_count + length(iter)
    train <- .data[1:(length(train_index_int) + iter),]
    fit <- fit_ts(.data = train, ts_model = ts_model, parameter = parameter)
    fitted_values <- predict_ts(fit, .data = train
                                , optim_conf = optim_conf
                                , parameter = parameter
                                , ts_model = ts_model
                                , add_fitted = TRUE
                                , type = "response")
    predicted <- predict_ts(fit = fit, .data = .data
                            , parameter = parameter
                            , optim_conf = optim_conf
                            , ts_model = ts_model
                            , add_fitted = FALSE
                            , type = "response")
    error <- .data[test_index_int,]$y_var - predicted
    out <- list(fit = fit
                , fitted_values = fitted_values
                , predicted = predicted
                , error = error
                , observed = .data[test_index_int,][["y_var"]])
    return(out)
  })
    
  optim_out_t <- transpose(optim_out)
  fitted_matrix <- plyr::ldply(optim_out_t$fitted_values, rbind)
  error_matrix <- matrix(unlist(optim_out_t$error), ncol = optim_conf$test_size, byrow = T)
  predicted_matrix <- matrix(unlist(optim_out_t$predicted), ncol = optim_conf$test_size, byrow = T)
  observed_matrix <- matrix(unlist(optim_out_t$observed), ncol = optim_conf$test_size, byrow = T)
  mape_matrix <- abs(error_matrix)/predicted_matrix
  spa_matrix <- observed_matrix/predicted_matrix
  mape_vec <- abs(diag(error_matrix))/diag(observed_matrix)
  spa_vec <- diag(observed_matrix)/diag(predicted_matrix)
  mape <- sum(abs(diag(error_matrix)))/sum(diag(observed_matrix))
  spa <- sum(diag(observed_matrix))/sum(diag(predicted_matrix))
  mse <- sum((diag(error_matrix))^2)/optim_conf$test_size
  mae <- sum(abs(diag(error_matrix)))/optim_conf$test_size
    
  optim_out <- list(
    model = ts_model
    , parameter = unlist(modifyList(parameter[[ts_model]], list(grid = NULL)))
    , key = attributes(.data)[["key"]]
    , fitted_matrix = round(fitted_matrix, 3)
    , error_matrix = round(error_matrix, 3)
    , predicted_matrix = round(predicted_matrix, 3)
    , mape_matrix = round(mape_matrix, 3)
    , spa_matrix = round(spa_matrix, 3)
    , mape_vec = round(mape_vec, 3)
    , spa_vec = round(spa_vec, 3)
    , mape = round(mape, 3)
    , spa = round(spa, 3)
    , mse = round(mse, 3)
    , mae = round(mae, 3)
  )
    
  if(export_fit == TRUE){
    optim_out <- append(optim_out, values = list(fit = optim_out_t$fit), after = 0)
    class(optim_out) <- c("list", "optim_ts")
    optim_out
  } else {
    class(optim_out) <- c("list", "optim_ts")
    optim_out
  }
}


#' Hyperparameter sample generator
#'
#' @param ts_model string
#' @param parameter list: initial set of hyperparameters.
#'
#' @return
#' @import tidyverse
#' @import utils
#' @export
#'
#' @examples
get_hyperpar_sample <- function(ts_model, parameter){
  # hyperparameter sample ---------------------------------------------------
  
  if(ts_model %in% c("gam", "glmnet", "glm", "arima")){
    par_sample <- parameter[[ts_model]]$grid %>% 
      mutate(index = 1:n(), .before = 1) %>%
      sample_frac(parameter[[ts_model]]$random_search, replace = F)
  } else if(ts_model == "ets"){
    par_sample <- parameter[[ts_model]]$ets %>% 
      enframe(name = "index", value = "ets")
  } else if(ts_model == "croston"){
    if(parameter[[ts_model]]$default_alpha == T){
      par_sample <- tibble(index = 1, alpha = .1)
    } else {
      par_sample <- parameter[[ts_model]]$grid %>% 
        enframe(name = "index", value = "alpha") %>%
        sample_frac(parameter[[ts_model]]$random_search, replace = F)
    }
  }
  
  if(ts_model == "gam"){
    map(1:nrow(par_sample), ~{
      tmp <- modifyList(parameter
                 , list(gam = modifyList(x = parameter$gam
                                         , list(trend_decay = par_sample$trend_decay[.x]
                                                #, time_weight = par_sample$time_weight[.x]
                                                , grid = FALSE)
                                         )
                 )
      )
      tmp[setdiff(names(tmp), ts_model)] <- NULL
      tmp
      }
    )
  } else if(ts_model == "glmnet"){
    
    map(1:nrow(par_sample), ~{
      tmp <- modifyList(parameter
                 , list(glmnet = modifyList(x = parameter$glmnet
                                            , list(time_weight = par_sample$time_weight[.x]
                                                   , trend_decay = par_sample$trend_decay[.x]
                                                   , alpha = par_sample$alpha[.x]
                                                   , grid = FALSE)
                                            )
                 )
      )
      
      tmp[setdiff(names(tmp), ts_model)] <- NULL
      tmp
      
    })
  } else if(ts_model == "glm"){
    map(1:nrow(par_sample), ~{
      tmp <- modifyList(parameter
                 , list(glm = modifyList(x = parameter$glm
                                         , list(time_weight = par_sample$time_weight[.x]
                                                , trend_decay = par_sample$trend_decay[.x]
                                                , grid = FALSE)
                                         )
                 )
      )
      tmp[setdiff(names(tmp), ts_model)] <- NULL
      tmp
    }
    )
  } else if(ts_model == "arima"){
    
    map(1:nrow(par_sample), ~{
      tmp <- modifyList(parameter
                        , list(arima = modifyList(x = parameter$arima
                                                  , list(pdq = c(par_sample$p[.x]
                                                                 , par_sample$d[.x]
                                                                 , par_sample$q[.x]
                                                                 , par_sample$P[.x]
                                                                 , par_sample$D[.x]
                                                                 , par_sample$Q[.x])
                                                         , grid = FALSE))))
      tmp[setdiff(names(tmp), ts_model)] <- NULL
      return(tmp)
    })
  } else if(ts_model == "ets"){
    map(1:nrow(par_sample), ~{
      tmp <- modifyList(parameter
                        , list(ets = modifyList(x = parameter$ets
                                                  , list(ets = c(par_sample$ets[.x])
                                                         , grid = FALSE))))
      tmp[setdiff(names(tmp), ts_model)] <- NULL
      return(tmp)
    })
  } else if(ts_model == "croston"){
    map(1:nrow(par_sample), ~{
      tmp <- modifyList(parameter
                        , list(croston = modifyList(x = parameter$croston
                                                , list(alpha = c(par_sample$alpha[.x])
                                                       , grid = FALSE))))
      tmp[setdiff(names(tmp), ts_model)] <- NULL
      return(tmp)
    })
  } else {
    message("NO MODEL")
  }
}

#' Default hyperparameter list by time series model
#' 
#' It's an function without arguments just to prompt user defined hyperparameter space and configuration.
#'
#' @return
#' @export
#'
#' @examples
get_default_hyperpar <- function(){
  list(gam = list(smoothed_features = list(trend = list(k = NA, bs = "tp"))
                  , formula = NULL
                  , excluded_features = list()
                  , time_weight = 1
                  , trend_decay = 1
                  , link_function = "gaussian"
                  , grid = tibble(trend_decay = c(0.7,.75,.8,.85,.9,.95,.99,1)
                                  #, time_weight = seq(from = 1, to = 1, by = 0.025)
                  )
                  , random_search = 1)
       , glm = list(formula = NULL
                    , excluded_features = NULL
                    , time_weight = 1
                    , trend_decay = 1
                    , link_function = "gaussian"
                    , grid = tidyr::expand_grid(time_weight = seq(from = 0.8
                                                           , to = 1, by = 0.025)
                                         , trend_decay = c(0.7,.75,.8,.85,.9,.95,.99,1))
                    , random_search = .4)
       , glmnet = list(alpha = .9
                       , lambda = numeric()
                       , time_weight = .95
                       , trend_decay = .97
                       , excluded_features = list()
                       , formula = NULL
                       , metric_lambda_optim = "mae"
                       , link_function = "gaussian"
                       , grid = tidyr::expand_grid(time_weight = seq(from = 0.8
                                                              , to = 1, by = 0.025)
                                            , trend_decay = c(0.7, 0.8, 0.9
                                                              , 0.95, 0.99,1)
                                            , alpha = seq(from = 0
                                                          , to = 1, by = 0.25))
                       , random_search = .3
                       , seed = 123)
       , arima = list(search_seasonal = TRUE
                      , auto_arima = FALSE
                      , random_search = .4
                      , pdq = c(1, 0, 0, 0, 0, 0)
                      , grid = tidyr::expand_grid(p = 0:2, d = 0:1, q = 0:2
                                           , P = 0:1, D = 0:0, Q = 0:1))
       , croston = list(alpha = 0.1
                        , grid = seq(from = 0, to = 1, by = .1)
                        , random_search = .5
                        , default_alpha = F)
       , ets = list(ets = "ZZZ")
  )
}

#' Default optimization configuration
#'
#' @param test_size integer: number of periods to be considered to test data.
#' @param lag integer: number of periods ahead to predict.
#' @param optim_profile string: name of the optimization profile {light, medium, complete} 
#'
#' @return
#' @import utils
#' @export
#'
#' @examples
get_default_optim_conf <- function(test_size = NULL, lag = NULL, optim_profile = NULL){
  
  new_optim_conf <- list(test_size = test_size, lag = lag, optim_profile = optim_profile)
  new_optim_conf <- new_optim_conf[!sapply(new_optim_conf, is.null)]
  optim_default_conf <- list(test_size = 6
                             , lag = 3
                             , optim_profile = "light")
  if(length(new_optim_conf)>0){
    modifyList(x = optim_default_conf
               , val = new_optim_conf)
  } else {
    optim_default_conf
  }
}


