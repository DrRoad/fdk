# GAM --------------------------------------------------------

fit_gam <- function(.data, optim = list(), parameter = list()){
  
  fit_gam_int <- function(.data, parameter){
    
    key_int <- attributes(.data)[["key"]]
    features <- setdiff(names(.data), c("date_var", "y_var"))
    features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(.data[[x]])))]
    features_factor <- setdiff(features, features_cont)
    
    # Time weight vector ---------------------------------------------------------
    
    time_weights_tmp <- get_time_weights(y_var = .data$y_var
                                         , time_weight = parameter$gam$time_weight)
    
    if(.log[[key_int]]$dates_check$n_dates < 13){
      gam_formula <- as.formula("y_var ~ trend")
    } else {
      # Replace formula ---------------------------------------------------------
      
      if(length(parameter$gam$formula) != 0){
        gam_formula <- formula
      } else {
        # Smoothed features -------------------------------------------------------
        
        if(any(is.null(names(parameter$gam$smoothed_features)))){
          sf_formula = ""
        } else {
          sf_formula <-   plyr::ldply(parameter$gam$smoothed_features, unlist) %>% 
            transmute(sf = paste0("s(", .id, ", k = ", k, ", bs = '", bs, "')")) %>% 
            pull() %>% 
            str_remove_all(pattern = "k = NA, ")
        }
        
        
        # Linear features ---------------------------------------------------------
        
        lf <- setdiff(features, c(names(parameter$gam$smoothed_features)
                                  , parameter$gam$excluded_features))
        if(length(lf) > 0){
          lf_formula <- lf
        } else {
          lf_formula <- ""
        }
        
        # Formula -----------------------------------------------------------------
        
        gam_formula <- as.formula(paste0(c("y_var ~ 1"
                                           , sf_formula
                                           , lf_formula)
                                         , collapse = " + "))
      }
    }
    
    # Fitting -----------------------------------------------------------------
    
    gam(formula = gam_formula
        , family = parameter$gam$link_function
        , data = .data
        , method = "REML")
  }
  

  # Optimizer ---------------------------------------------------------------
  
  if(sum(names(optim) %in% c("test_size", "lag_ref")) != 2){
    output <- fit_gam_int(.data = .data, parameter = parameter)
    #class(output) <- "fit_ts"
  } else {
    train_index <- 1:(nrow(.data) - (test_size + lag_ref) + 1)
    test_index <- (nrow(.data) - test_size + 1):nrow(.data)
    
    output <- map(1:test_size, function(x){
      fit <- fit_gam_int(.data = .data[1:(length(train_index) + x),], parameter = parameter)
      fitted <- fitted(fit)
      predicted <- predict(object = fit, newdata = .data[test_index,], type = "response")
      error <- .data[test_index,]$y_var - predicted
      out <- list(fit = fit, fitted = fitted, predicted = predicted
                  , error = error
                  , observed = .data[test_index,][["y_var"]])
    })
    
    class(output) <- "optim_ts"
    output <- optim_tidy(.optim_ts = output, optim = optim)
  }
  
  return(output)
}
