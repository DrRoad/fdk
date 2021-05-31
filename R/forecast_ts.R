#' Generate forecast
#'
#' @param .fit fit object
#' @param new_data tibble
#' @param horizon numeric
#' 
#' @importFrom purrr map_dfc
#'
#' @return
#' @export
#'
#' @examples
forecast_ts <- function(.fit, new_data = NULL, horizon = 12){
  
  fdk_class <- attributes(.fit)[["fdk_class"]]
  if(is.null(fdk_class) == F){
    if(fdk_class == "optim_ts"){
      key <- attributes(.fit$fit[[1]])[[".log"]][["key"]]
    }
  } else {
    key <- attributes(.fit)[[".log"]][["key"]]
  }
  
  forecast_ts_int <- purrr::possibly(function(.fit, new_data, horizon){
    key <- attributes(.fit)[[".log"]][["key"]]
    parameter <- attributes(.fit)[[".log"]]
    
    if(horizon < 12){
      horizon_pad <- seq(from = 1, to = (12 - horizon))
    } else {
      horizon_pad <- 0
    }
    original_length <- .log[[key]]$dates_check$n_dates
    date_var_int <- seq.Date(from = (.log[[key]]$dates_check$date_range[[2]] + months(1))
                             , length.out = horizon + max(horizon_pad)
                             , by = as.character(.log$prescription$freq_name[[1]]))
    if(length(horizon_pad) > 0){
      date_var_int_exc <- rev(date_var_int)[horizon_pad]
    } else {
      date_var_int_exc <- NA
    }
    
    if(any(class(.fit) %in% c("glm", "gam", "glmnet"))){
      suppressMessages(
        {
          reg_place <- tryCatch(
            {
              map_dfc(.log[[key]]$long2wide$regressor_names, ~rep(0, horizon + max(horizon_pad))) %>% 
                setNames(.log[[key]]$long2wide$regressor_names)
            }
            , error = function(err) NULL
          )
        }
      )
      
      new_data_int <- tibble(date_var = date_var_int) %>% 
        mutate(trend = seq(from = original_length + 1
                           , to = original_length + horizon + max(horizon_pad))
               , month_seas = months(date_var, abbreviate = T)) %>% 
        bind_cols(reg_place)
      
      trend_decay_tmp <- get_trend_decay(y_var_length = (min(new_data_int$trend) - 1)
                                         , trend_decay = parameter$parameter$trend_decay
                                         , horizon = length(new_data_int$trend)) %>% 
        round(2)
      
      new_data_int <- new_data_int %>% 
        mutate(trend = trend_decay_tmp)
      
      if(any(class(.fit) %in% c("glmnet", "glm"))){
        features_matrix <- new_data_int %>%
          #dplyr::select(all_of(attributes(.fit)[[".log"]][["features"]])) %>% 
          fastDummies::dummy_cols(select_columns = attributes(.fit)[[".log"]][["features_factor"]]
                                  , remove_selected_columns = T
                                  , remove_first_dummy = F) %>% 
          dplyr::select(all_of(attributes(.fit)[[".log"]][["features_matrix_names"]]))
        
        if(any(class(.fit) %in% c("glmnet"))){
          features_matrix <- features_matrix %>% 
            as.matrix()
        }
      }
    }
    
    
    if(any(class(.fit) %in% c("ets", "Arima", "ARIMA"))){
      forecast::forecast(object = .fit, h = (horizon +  max(horizon_pad)))[["mean"]] %>% 
        as.numeric() %>% 
        enframe(value = "forecast") %>% 
        mutate(key = key
               , date_var = date_var_int) %>% 
        dplyr::select(date_var, forecast) %>% 
        filter(!(date_var %in% date_var_int_exc))
    } else if(any(class(.fit) %in% c("glmnet"#, "glm"
                                     ))){
      
      predict(.fit, features_matrix, type = "response") %>% 
        as.numeric() %>% 
        enframe(value = "forecast") %>% 
        mutate(key = key
               , date_var = date_var_int) %>% 
        dplyr::select(date_var, forecast) %>% 
        filter(!(date_var %in% date_var_int_exc))
    } else if(any(class(.fit) %in% c("gam", "glm"))){
      
      new_data_int %>% 
        dplyr::select(all_of(attributes(.fit)[[".log"]][["features"]])
                      , -any_of("date_var")) %>%
        predict(object = .fit, ., type = "response") %>%
        as.numeric() %>%
        enframe(value = "forecast") %>% 
        mutate(key = key
               #, forecast = forecast + .fit$coefficients[[1]]
               , date_var = date_var_int) %>% 
        dplyr::select(date_var, forecast) %>% 
        filter(!(date_var %in% date_var_int_exc))
    } else if(any(class(.fit) %in% c("forecast"))){
      rep(.fit$mean[[1]], (horizon +  max(horizon_pad))) %>% 
        as.numeric() %>% 
        enframe(value = "forecast") %>% 
        mutate(key = key
               , date_var = date_var_int) %>% 
        dplyr::select(date_var, forecast) %>% 
        filter(!(date_var %in% date_var_int_exc))
    }
  }, otherwise = NULL)
  
  
  if(is.null(fdk_class) | fdk_class == "fit"){
    out <- forecast_ts_int(.fit = .fit
                           , new_data = new_data
                           , horizon = horizon)
    out %>% 
      structure(fdk_class = "forecast_ts"
                , .log = list(key = key))
  } else if(fdk_class == "optim_ts"){
    out <- .fit %>% 
      #slice(1) %>% 
      mutate(forecast = map(fit, ~forecast_ts_int(.fit = .x
                                                  , new_data = new_data
                                                  , horizon = horizon)))
    
    out %>% 
      structure(fdk_class = c("forecast_ts", "optim_ts")
                , .log = list(key = key))
  }

  # Return ------------------------------------------------------------------

}