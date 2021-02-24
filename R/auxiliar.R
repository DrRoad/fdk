# Regression helpers -------------------------------

#' Generate trend discounts
#' 
#' Regression based models use a linear trend to account for the change in level over time. 
#' In practical terms, it is measured as a vector of equidistant integers.
#' Often, the trend component can significantly impact the forecast in the long run. One way to solve
#' this issue is to apply a "discount" on the trend vector, henceforth, reducing the marginal effect on the 
#' predictions.
#' @param y_var_length Numeric. Length of the time series
#' @param trend_discount Numeric. How rapidly the trend reach the stability
#' @param horizon Numeric. How far in time to produce trend discounts.
#' @param lag Numeric. Lag to be used for cross-validation purposes.
#'
#' @author Obryan Poyser
#' @return Numerical vector.
#' @export
#'
#' @examples
#' \dontrun{
#' get_trend_discounts()
#' }
get_trend_decay <- function(y_var_length, trend_decay, horizon=NULL, lag = NULL){
  if(length(y_var_length)>1){
    y_var_length <- length(y_var_length)
  } else {
    y_var_length
  }
  if(length(trend_decay)!=1) stop("Only one trend discount value should be provided")
  if(is.null(lag)==F & length(lag) != 1) stop("Only one lag value should be provided")
  if(is.null(horizon)==T){
    trend_decay <- y_var_length + cumsum(trend_decay^(0:(y_var_length-1)))
    if(is.null(lag)==T){
      return(trend_decay)
    } else {
      return(trend_decay[[lag]])
    }
  } else {
    trend_decay <- y_var_length + cumsum(trend_decay^(0:(horizon-1)))
    return(trend_decay)
  }
}

#' Generate time weights
#' 
#' Method to define data points' weights for regression based models.
#'
#' @param y_var numeric. Time series vector data.
#' @param time_weight Numeric. How rapidly recent observations weight for estimation.
#' @author Obryan Poyser
#' @return Numerical vector.
#' @export
#'
#' @examples
#' \dontrun{
#' get_time_weights()
#' }
get_time_weights <- function(y_var, time_weight){
  if(length(time_weight)!=1) stop("Only one time weight value should be provided")
  time_weight^(length(1:length(y_var))-(1:length(y_var)))^2
}

#' Generate regression matrix for regression based models
#' 
#' This function takes the metadata from the fit to generate a template of the
#' dependent variables to make forecast.
#'
#' @param .fit_output Inherited data and meta-data from a model fit.
#' @param x_data Optional DataFrame to use as design matrix.
#' @param horizon Numeric. How far in time to produce a synthetic design matrix.
#' 
#' @import dplyr
#' @return data-frame or tibble
#' @export
#'
#' @examples
#' \dontrun{
#' make_reg_matrix()
#' }
make_reg_matrix <- function(.fit_output, x_data = NULL, horizon = NULL){
  
  prescription <- attributes(.fit_output)[["prescription"]]
  
  if(is.null(x_data) == TRUE){
    #message("Synthetic data")
    
    reg_matrix_tmp <- tibble(.rows = horizon)
    
    for(i in 1:length(.fit_output$parameter$fit_summary$x_var)){
      reg_matrix_tmp[.fit_output$parameter$fit_summary$x_var[i]] <- 0
    }
    
    reg_matrix_tmp <- reg_matrix_tmp %>% 
      mutate(date = seq.Date(from = as.Date(prescription$max_date + months(1)) # expand for week
                             , length.out = horizon
                             , by = "month")
             , seasonal_var = factor(months(date, abbreviate = TRUE), levels = month.abb)
             , trend = get_trend_discounts(y_var_length = .fit_output$parameter$fit_summary$data_size
                                           , trend_discount = .fit_output$parameter$trend_discount
                                           , horizon = horizon)) %>% 
      select(.fit_output$parameter$fit_summary$x_var)
  
    if(.fit_output[["model"]] == "glmnet"){
      reg_matrix_tmp %>% 
        fastDummies::dummy_cols(select_columns = .fit_output$parameter$fit_summary$factor_var
                                , remove_selected_columns = T) %>%
        select(.fit_output$parameter$fit_summary$x_var_matrix) %>% 
        as.matrix()
    } else if(.fit_output[["model"]] == "glm"){
      return(reg_matrix_tmp)
    }
  } else if(is.null(x_data) == FALSE) {
    #message("New x_data")
    if(.fit_output[["model"]] == "glmnet"){
      x_data %>% 
        select(.fit_output$parameter$fit_summary$x_var) %>% 
        fastDummies::dummy_cols(select_columns = .fit_output$parameter$fit_summary$factor_var
                                , remove_selected_columns = T) %>%
        select(.fit_output$parameter$fit_summary$x_var_matrix) %>% 
        mutate(trend = get_trend_discounts(y_var_length = .fit_output$parameter$fit_summary$data_size
                                           , trend_discount = .fit_output$parameter$trend_discount
                                           , lag = prescription$lag)) %>% 
        as.matrix()
    } else if(.fit_output[["model"]] == "glm"){
      x_data %>% 
        select(.fit_output$parameter$fit_summary$x_var) %>% 
        mutate(trend = get_trend_discounts(y_var_length = .fit_output$parameter$fit_summary$data_size
                                           , trend_discount = .fit_output$parameter$trend_discount
                                           , lag = prescription$lag))
    }
  }
}

# Splits -------------------------------

#' Automatic Time Series Cross-Validation split
#' 
#' Time series CV split heuristics should keep temporal dependencies, henceforth, the sample should not 
#' be "shuffled" into train and test. This function uses a different strategy to define the test and train sets
#' maintain the order of the data.
#'
#' @param .data DataFrame, tibble or tsibble structures.
#' @param test_size Numeric. How many periods will be use to asses the forecast accuracy.
#' @param lag Numeric. How many periods ahead to start the test size. 
#'
#' @return Nested data-frames or tibbles.
#' @export
#'
#' @examples
#' \dontrun{
#' split_ts()
#' }
split_ts <- function(.data, test_size, lag){
  split_ts_helper <- function(.data, test_size, lag, iter){
    attr(.data, "prescription")[["lag"]] <- lag
    attr(.data, "prescription")[["iter"]] <- iter
    ts_len <- length(.data[,1][[1]])
    train <- .data[1:(ts_len - test_size - lag + iter),]
    test <- .data[ts_len - test_size + iter,]
    list(train = train, test = test)
  }
  map(1:test_size, ~split_ts_helper(.data = .data, test_size = test_size, lag = lag, iter = .x))
}

#' Accuracy metrics
#' 
#' Mean Average Prediction Error forecast accuracy metric
#'
#' @param y_var_true Numeric. Observed value.
#' @param y_var_pred Numeric. Predicted value.
#' @param metric String. Name of the accuracy metric.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' \dontrun{
#' accuracy_metric()
#' }
accuracy_metric <- function(y_var_true, y_var_pred, metric = "mape"){
  if(metric == "mape"){
    if((is.na(y_var_true) == T | (round(y_var_true, 0) == 0) == TRUE)){
      mape_tmp <- NA
    } else {
      mape_tmp <- abs(y_var_true - y_var_pred)/y_var_true
    }
    return(mape_tmp)
  } else if(metric == "fa"){
    if((is.na(y_var_true) == T | (round(y_var_true, 0) == 0) == TRUE)){
      mape_tmp <- NA
    } else {
      mape_tmp <- (y_var_true - y_var_pred)/y_var_true
    }
  }
}

#' Updating parameters given a grid.
#' 
#' This function updates the parameter list by a set of values coming from a data-frame or tibble. 
#' It is a fundamental for glmnet, glm and arima models.
#'
#' @param old_parameter List. Parameters to be used to estimate model or define the job to be done.
#' @param new_parameter List or data-frame. Parameters that will replace the original list.
#' @param model String. Model where the parameters will be replaced.
#' @param optim Logical. Wether of not the results will be used for hyperparameter tuning of GLMNET models.
#'
#' @return List of updated parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' update_parameter()
#' }
update_parameter <- function(old_parameter, new_parameter, model, optim = FALSE){
  parameter_tmp <- old_parameter
  if(model == "glmnet"){
    parameter_tmp$glmnet$time_weight <- new_parameter$time_weight
    parameter_tmp$glmnet$trend_discount <- new_parameter$trend_discount
    parameter_tmp$glmnet$alpha <- new_parameter$alpha
    if(optim == TRUE){
      parameter_tmp$glmnet$lambda <- new_parameter$lambda
      parameter_tmp$glmnet$job$optim_lambda <- FALSE
    }
  } else if(model == "glm"){
    parameter_tmp$glm$time_weight <- new_parameter[["time_weight"]]
    parameter_tmp$glm$trend_discount <- new_parameter[["trend_discount"]]
  } else {
    stop("No model has been set")
  }
  return(parameter_tmp)
}

summary_ts <- function(.data){
  if("optim_ts" %in% class(.data)){
    mape <- .data$mape %>% format(digits = 2, nsmall = 2)
    spa <- .data$spa %>% format(digits = 2, nsmall = 2)
    mse <- .data$mse %>% format(digits = 2, nsmall = 2)
    mae <- .data$mae %>% format(digits = 2, nsmall = 2)
    mape_i <- .data$mape_vec %>% format(digits = 2, nsmall = 2)
    spa_i <- .data$spa_vec %>% format(digits = 2, nsmall = 2)
    error <- diag(.data$error_matrix) %>% abs() %>%  format(digits = 2, nsmall = 2)
    
    # c1 <- c("AGGREGATED","", "{iter}", 1:6)
    # c2 <- c(mape,"", "", mape_i)
    # c3 <- c(spa,"", "", spa_i)
    # c4 <- c("","", "", error)
    # 
    # matrix(c(c1, c2, c3, c4), nrow = 9) %>% 
    #   knitr::kable(col.names = c("", "MAPE", "SPA", "ERROR ABS")
    #                , digits = 2, format = "simple")
    
    cat(c(paste0("OPTIMIZATION METRICS ", "#######")
          , paste0(rep("#", 28), collapse = "")
          , toupper(.data$model)
          , mape, spa, mse, mae), fill = 1
        , labels = paste0("##", c("", "", " MODEL ="
                                  ," MAPE ="
                                  , " SPA ="
                                  , " MSE ="
                                  , " MAE =")))
    
    #return(.data)
    
    #cat(paste0("## Optimization results ", paste0(rep("#", 40), collapse = "")), "\n")
    # vec <- c(paste0("MAPE = ", mape), paste0("SPA  = ", spa))
    #cat(vec, fill = 2, labels = "##")
    #cat(c(mape, spa))
    # cat(" ## MAPE = ", mape, "\n"
    #     , "## MAPE iteration = ", mape_i, "\n"
    #     , "## SPA = ", spa, "\n"
    #     , "## SPA iteration = ", spa_i
    #     , fill = T
    #     , labels = )
  }
}

# Input transformers

## Inherited from OC -----------------------------------------------------------

#' Reverse scope
#' 
#' This function collects paths or RDatas given countries and GBU
#'
#' @param countries 
#' @param gbus 
#' @param date_cycle 
#' @param db 
#' @param read_db 
#'
#' @return
#' @export
#'
#' @examples
reverse_scope <- function(countries, gbus, date_cycle, db = NULL, read_db = FALSE){
  root_input <- "//E21flsbcnschub/BCN_SC_HUB/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/"
  
  input_path <- readRDS("data/loc_mapping.rds") %>% 
    janitor::clean_names() %>% 
    dplyr::filter(country %in% countries) %>% 
    rowwise() %>% 
    mutate(path_input = paste0(root_input
                               , "Outputs/"
                               , gbus, "/"
                               , mco, "/"
                               , country, "/"
                               , format(as.Date(date_cycle)
                                        , format = "%b - %Y"), "/")) %>% 
    ungroup()
  
  if(is.null(db)==F){
    paths <- paste0(input_path$path_input,"RData/", db, ".RData")
  } else {
    paths <- input_path$path_input
  }
  
  if(read_db == T){
    map(paths, ~rdata2obj(.x)) %>% 
      bind_rows()
  } else {
    paths
  }
}


#' RData 2 Object
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
rdata2obj <- function(path){
  tmp_env <- new.env()
  load(path, envir = tmp_env)
  as.list(tmp_env)[[1]]
}


# Input data --------------------------------------------------------------

get_oc_data <- function(db = list(), countries, gbus, date_cycle){
  admitted <- c("full_sales", "full_forecast", "forecast_item_info", "regressor")
  
  if(!any(unlist(db) %in% admitted)){
    stop(paste0("Only the following DB's are admitted: ", paste0(admitted, collapse = ", ")))
  }
  
  
  count <- 0
  all <- length(db)
  data <- map(unlist(db), ~{
    count <<- count + 1
    message(paste0("Reading: ", count, "/", all))
    reverse_scope(
      countries = countries
      , gbus = gbus
      , date_cycle = date_cycle
      , db = .x
      , read_db = T)
    }
    )
  names(data) <- unlist(db)
  if("regressor" %in% names(data)){
    data[["regressor"]] <- data[["regressor"]] %>% 
      dplyr::select(forecast_item, reg_name, reg_date, reg_value) %>% 
      mutate(reg_name = str_remove(string = reg_name
                                   , pattern = paste0(forecast_item, " - |-"))
             ) %>% 
      dplyr::distinct()
  }
  data
}


