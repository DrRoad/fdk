#' Check inputs
#'
#' Check for data structure compliance.
#' @param .data DataFrame
#'
#' @return
#' @export
#' @noRd
#' @examples
check_inputs <- function(.data){
  # Colnames checks
  if(colnames(data)[1]!="key"){
    print("Check first column name please")
  }
  if(colnames(data)[2]!="y"){
    print("Check second column name please")
  }
  if(colnames(data)[3]!="date"){
    print("Check third column name please")
  }
  if(colnames(data)[4]!="reg"){
    print("Check fourth column name please")
  }
  # Format checks
}

#' Detect seasonality
#' 
#' This is a consensus tool to decide if a time series exhibit regular patterns over time.
#' 
#' The consensus is formed as Kruskal test, seasonal strength, and statistical significance
#' of a linear regression model. Rule: if 2 out of 3 methods show evidence in favor of seasonality
#' then, the data will employ seasonal models to generate the forecast.
#'
#' @param data numeric. Time series data
#'
#' @return
#' @export
#'
#' @examples
detect_seasonality <- function(data){
  # Aux functions
  seas_test_kw <- function(ts){
    kwtest <- kw(ts)
    if(kwtest$Pval < 0.05){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  seas_test_ftrs <- function(ts){
    # Calculate Seasonality Strength and reformat
    features <- as.data.frame(t(stl_features(ts)))
    if(features$seasonal_strength>0.7){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  seas_test_lm <- function(ts){
    # Add 1 diff
    lm.1=lm(y~factor(time_seas),data=ts)
    p.vals=summary(lm.1)
    p.vals.lt<-pf(p.vals$fstatistic[1], p.vals$fstatistic[2], # Compute p-value from the F-statistics
                  p.vals$fstatistic[3], lower.tail=FALSE) # and degree of freedom
    if(p.vals.lt < 0.10 & !is.nan(p.vals.lt)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  # Main test
  Count <- 0
  # Test1: Kruskal-Wallis
  if(seas_test_kw(data[[1]])){
    Count <- Count + 1
  }
  # Test2: Time Series Features
  if(seas_test_ftrs(data[[1]])){
    Count <- Count + 1
  }
  # Test3: Linear Model
  if(seas_test_lm(data[[2]])){
    Count <- Count + 1
  }
  # Final Decision
  if(Count>=2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


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
#' @return
#' @export
#'
#' @examples
get_trend_discounts <- function(y_var_length, trend_discount, horizon=NULL, lag = NULL){
  if(length(y_var_length)>1){
    y_var_length <- length(y_var_length)
  } else {
    y_var_length
  }
  if(length(trend_discount)!=1) stop("Only one trend discount value should be provided")
  if(is.null(lag)==F & length(lag) != 1) stop("Only one lag value should be provided")
  if(is.null(horizon)==T){
    trend_discount <- y_var_length + cumsum(trend_discount^(0:(y_var_length-1)))
    if(is.null(lag)==T){
      return(trend_discount)
    } else {
      return(trend_discount[[lag]])
    }
  } else {
    trend_discount <- y_var_length + cumsum(trend_discount^(0:(horizon-1)))
    return(trend_discount)
  }
}

#' Generate time weights
#'
#' @param y_var numeric. Time series vector data.
#' @param time_weight numeric. How rapidly recent observations weight for estimation.
#' @author Obryan Poyser
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
make_reg_matrix <- function(.fit_output, x_data = NULL, horizon = NULL){
  
  prescription <- attributes(.fit_output)[["prescription"]]
  
  if(is.null(x_data) == TRUE){
    
    tmp_tibble <- tibble(.rows = horizon)
    
    for(i in 1:length(.fit_output$parameter$fit_summary$x_var)){
      tmp_tibble[.fit_output$parameter$fit_summary$x_var[i]] <- 0
    }
    
    tmp_tibble %>% 
      mutate(date = seq.Date(from = as.Date(prescription$max_date + months(1)) # expand for week
                             , length.out = horizon
                             , by = "month")
             , seasonal_var = factor(months(date, abbr=TRUE), levels = month.abb)
             , trend = get_trend_discounts(y_var_length = .fit_output$parameter$fit_summary$train_size
                                           , trend_discount = .fit_output$parameter$trend_discount
                                           , horizon = horizon)) %>% 
      select(.fit_output$parameter$fit_summary$x_var) %>% 
      fastDummies::dummy_cols(select_columns = .fit_output$parameter$fit_summary$factor_var
                              , remove_selected_columns = T) %>%
      select(.fit_output$parameter$fit_summary$x_var_matrix) %>% 
      as.matrix()
    
  } else {
    x_data %>% 
      select(.fit_output$parameter$fit_summary$x_var) %>% 
      fastDummies::dummy_cols(select_columns = .fit_output$parameter$fit_summary$factor_var
                              , remove_selected_columns = T) %>%
      select(.fit_output$parameter$fit_summary$x_var_matrix) %>% 
      mutate(trend = get_trend_discounts(y_var_length = .fit_output$parameter$fit_summary$train_size
                                         , trend_discount = .fit_output$parameter$trend_discount
                                         , lag = prescription$lag)) %>% 
      as.matrix()
  }
}

# Splits -------------------------------

# Main ts_split function

#' Time Series Cross-Validation split
#' 
#' Time series CV split heuristics should keep temporal dependencies, henceforth, the sample should not 
#' be "shuffled" into train and test. This function uses a different strategy to define the test and train sets
#' maintain the order of the data.
#'
#' @param data DataFrame or tibble
#' @param test_size Numeric. How many periods will be use to asses the forecast accuracy.
#' @param lag Numeric. How many periods ahead to start the test size. 
#' @param i Numeric. Iteration.
#'
#' @return
#' @export
#'
#' @examples
ts_split_all <- function(data, test_size, lag, i){
  if(class(data)[1] == "ts"){
    ts_len <- length(data)
    train <- subset(data, end = ts_len-test_size-lag+i)
    test <- subset(data, start = ts_len-test_size+i, end = ts_len-test_size+i)
    train_test=list(train = train, test = test, pars = list(lag = lag, iter = i))
  }else{
    ts_len <- length(data[,1][[1]])
    train <- data[1:(ts_len-test_size-lag+i),]
    test <- data[ts_len-test_size+i,]
    train_test=list(train = train, test = test, pars = list(lag = lag, iter = i))
  }
}

#' Automatic Time Series Cross-Validation split
#'
#' @param .data DataFrame, tibble or tsibble structures.
#' @param test_size Numeric. How many periods will be use to asses the forecast accuracy.
#' @param lag Numeric. How many periods ahead to start the test size. 
#'
#' @return
#' @export
#'
#' @examples
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

#' MAPE
#' 
#' Mean Average Prediction Error forecast accuracy metric
#'
#' @param real Numeric. Observed value.
#' @param pred Numeric. Predicted value.
#'
#' @return
#' @export
#'
#' @examples
mape <- function(real, pred){
  return(round((abs(real-pred)/real),3))
}


#' Generate trend and seasonal components for regression based models
#'
#' @param .data DataFrame or tibble
#' @param date_var String. Column name of the time index variable
#' @param frequency Numeric. Time series frequency
#' @param bind Logical. Whether or not to bind the regressors to the original data. Default = TRUE.
#'
#' @return
#' @export
#'
#' @examples
get_design_matrix <- function(.data, date_var=NULL, freq=NULL, parameter = NULL, to_dummy = TRUE){
  
  if(is.null(attributes(.data)[["prescription"]])==FALSE){
    prescription <- attributes(.data)[["prescription"]]
    y_var <- prescription$y_var
    date_var <- prescription$date_var
    freq <- prescription$freq
  }
  
  if(freq == 12){
    reg_seasonal <- function(date) factor(months(as.Date(date), abbreviate = T), levels = month.abb)
  } else if(freq == 4){
    reg_seasonal <- function(date) factor(as.factor(quarters(as.Date(date), abbreviate = T)), levels = paste0("Q", 1:4))
  } else if(round(freq, 0) == 52){
    reg_seasonal <- function(date) factor(lubridate::week(date), levels = 1:53)
  }
  
  seasonal_var <- reg_seasonal(.data[[date_var]])
  trend <- 1:length(seasonal_var)
  
  .data_tmp <- .data %>% 
    bind_cols(tibble(trend = trend, seasonal_var = seasonal_var)) %>% 
    relocate("trend", "seasonal_var", .after = "y_var")
  
  na_exclude <- unique(c(prescription$key, y_var, date_var))
  reg_excluded <- unique(c(na_exclude, parameter[["glmnet"]][["job"]][["reg_excluded"]]))
  factor_var <- names(.data_tmp)[sapply(.data_tmp, function(x) ifelse(is.character(x) | is.factor(x), T, F))]
  reg_var <- names(.data_tmp)[!(names(.data_tmp) %in% reg_excluded)]
  
  if(to_dummy){
    .data_tmp[, reg_var] %>%
      dummy_cols(
        select_columns = factor_var, remove_first_dummy = T,
        remove_selected_columns = T
      ) %>%
      as.matrix()
  } else {
    .data_tmp
  }
}

update_parameter <- function(parameter, grid, model="glmnet"){
  if(model == "glmnet"){
    parameter$glmnet$time_weight <- grid$time_weight
    parameter$glmnet$trend_discount <- grid$trend_discount
    parameter$glmnet$alpha <- grid$alpha
    return(parameter)
  }
}

get_metric <- function(.forecast_output){
  .forecast_output %>% 
    mutate(mape_i = abs(.y_var_true - .y_var_pred)/.y_var_true) %>% 
    group_by(trend_discount, time_weight, alpha) %>% 
    summarise(mape = mean(mape_i)
              , lambda_median = median(lambda)
              , lambda_cov = sd(lambda)/mean(lambda)
              , .groups = "drop") %>% 
    arrange(mape, -lambda_cov) %>%
    select(1:3, lambda = lambda_median, cv_mape = mape) %>% 
    .[1,]
}
