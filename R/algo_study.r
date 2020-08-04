
# algo_study

algo_study <- function(data, models, seas = TRUE, parameters = NULL,
                       test_size = 6, lag = 4){

  # Train data --------------

  train <- data

  # # Check seasonality --------------
  #
  # if(is.null(seas)){
  #   seas <- detect_seasonality(train)
  # }

  # Results --------------

  results <- data.frame(matrix(nrow = 0,ncol = 6))
  colnames(results) <- c("model","time","predicted","real","mape","parameter")

  # Split Training Data & Run --------------

  for(i in 1:test_size){

    xd1 <- ts_split_1(train[[1]], test_size, lag, i) # Time Series
    xd2 <- ts_split_2(train[[2]], test_size, lag, i) # Tibble

    # Stat Models --------------

    # naive
    if("naive" %in% models){
      output <- get_naive(xd = xd1, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # snaive
    if("snaive" %in% models){
      output <- get_snaive(xd = xd1, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # croston
    if("croston" %in% models){
      output <- get_croston(xd = xd1, parameters = parameters$params_croston, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # ets
    if("ets" %in% models){
      output <- get_ets(xd = xd1, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # theta
    if("theta" %in% models){
      output <- get_theta(xd = xd1, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # arima
    if("arima" %in% models){
      output <- get_arima(xd = xd1, seas = seas, parameters = parameters$params_arima, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # tbats
    if("tbats" %in% models){
      output <- get_tbats(xd = xd1, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # nnetar
    if("nn" %in% models){
      output <- get_nn(xd = xd1, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

    # prophet
    if("prophet" %in% models){
      output <- get_prophet(xd = xd2, h = lag, mode = "sim")
      results <- rbind(results,output)
    }

  } # End Cv Loop

  # Machine Learning Models --------------

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

  # results --------------

  selected_config <- data.frame(frequency = frequency(train[[1]]),
                                allow_seas = seas,
                                buckets = length(train[[1]]),
                                test_size = test_size,
                                lag = lag)

  # Output --------------

  best_models <- results %>% group_by(model) %>%
    summarise(cv_mape = (sum(abs(real-predicted)))/sum(real))
  # mean_mape = mean(mape, na.rm = TRUE),
  # sd_mape = sd(mape, na.rm = TRUE))

  # If more than 1 model, arrange by mape --------------

  if(length(unique(best_models$model))>1){
    best_models <-  best_models %>% arrange(cv_mape)
  }

  return(list(selected_config,best_models))

}
