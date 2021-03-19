
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
      parameter_list <- update_hyperpar(ts_model = ts_model_i, parameter = parameter)
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
#' @param .data 
#' @param ts_model 
#' @param optim_conf 
#' @param parameter 
#' @param export_fit 
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
  # Rules -------------------------------------------------------------------
  
  interm_rule <- (sum(.data$y_var==0)/nrow(.data)>.3)
  size_rule <- (nrow(.data) - optim_conf$test_size - optim_conf$lag)<13
  ts_models_rule <- ts_model %in% c("glmnet", "gam", "glm", "arima")
  
  # What to do if the conditions are not met
  
  if((interm_rule | size_rule)# & ts_models_rule
  ){
    message(paste0("Too much intermittency or small sample to tune model <"
                   , toupper(ts_model), ">."))
  } else {
    
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
}


#' Hyperparameter updater
#'
#' @param ts_model 
#' @param parameter 
#'
#' @return
#' @export
#'
#' @examples
update_hyperpar <- function(ts_model, parameter){
  # hyperparameter sample ---------------------------------------------------
  
  if(ts_model %in% c("gam", "glmnet", "glm")){
    par_sample <- parameter[[ts_model]]$grid %>% 
      mutate(index = 1:n(), .before = 1) %>%
      sample_frac(parameter[[ts_model]]$random_search, replace = F)
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
  }
}
