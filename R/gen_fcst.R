
# gen_fcst

gen_fcst <- function(data, models, seas = TRUE, parameters = NULL, h = 36){

  # # Check seasonality --------------
  #
  # if(is.null(seas)){
  #   seas <- detect_seasonality(data)
  # }

  # Results --------------

  results <- data.frame(matrix(nrow = 0,ncol = 6))
  colnames(results) <- c("model","time","predicted","real","mape","parameters")

  # Models --------------

  xd1 <- data[[1]] # Time Series
  xd2 <- data[[2]] # Tibble

  # Stat Models --------------

  # naive/snaive
  if("naive" %in% models){
    output <- get_naive(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # snaive
  if("snaive" %in% models){
    output <- get_snaive(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # croston
  if("croston" %in% models){
    output <- get_croston(xd = xd1, parameters = parameters$params_croston, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # ets
  if("ets" %in% models){
    output <- get_ets(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # theta
  if("theta" %in% models){
    output <- get_theta(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # arima
  if("arima" %in% models){
    output <- get_arima(xd = xd1, seas = seas, parameters = parameters$params_arima, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # tbats
  if("tbats" %in% models){
    output <- get_tbats(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # nnetar
  if("nn" %in% models){
    output <- get_nn(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # prophet
  if("prophet" %in% models){
    output <- get_prophet(xd = xd2, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # Machine Learning Models

  # glmnet
  if("glmnet" %in% models){
    output <- optim_glmnet(data = train[[2]], hyperparam = hyperparam_list,
                           random_sample = .20, config = job_config)
  }

  # # mlr
  # if("mlr" %in% models){
  #   output <- get_mlr(xd2, seas, parameters)
  #   results <- rbind(results,output)
  # }

  # Return

  return(results)

}

