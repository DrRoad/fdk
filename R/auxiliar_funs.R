
# aux functions

# check inputs -------------------------------

check_inputs <- function(data){
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

# check seasonality -------------------------------

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

# tscut -------------------------------

tscut = function(time.series){
  if(length(time.series)<25){ # same as Kinaxis
    new.ts = time.series  
    new.ts = Winsorize(new.ts,na.rm = TRUE)
    if(sum(new.ts, na.rm = T) == 0){new.ts =time.series } # if outlier method changes all values to zero, revert back to original ts
    new.ts[new.ts<0]=0 # Coerce negative values to zero  
    return(new.ts = new.ts)
  }
  if(length(time.series)>=25){
    rstl = stl(time.series,s.window = "periodic",robust = T)
    resi = rstl$time.series[,3]
    resi.new = resi
    win.resi = Winsorize(resi,na.rm = T )
    resi.new[resi.new< min(win.resi) ] = min(win.resi)  #lower threshold
    resi.new[resi.new> max(win.resi) ] = max(win.resi)  #upper threshold
    adjustment = resi.new -resi            
    new.ts = time.series + adjustment
    if(sum(new.ts[!(is.na(new.ts))]) == 0){new.ts =time.series }    # if outlier method changes all values to zero, revert back to original ts
    new.ts[new.ts<0]=0         # Coerce negative values to zero 
    return(new.ts = new.ts)
  }
  return(new.ts = time.series)
}

# Regression helpers -------------------------------

get_trend_discounts <- function(data_length, trend_discount, horizon=NULL, lag = NULL){
  if(length(data_length)>1){
    data_length <- length(data_length)
  } else {
    data_length
  }
  if(length(trend_discount)!=1) stop("Only one trend discount value should be provided")
  if(is.null(lag)==F & length(lag) != 1) stop("Only one lag value should be provided")
  if(is.null(horizon)==T){
    trend_discount <- data_length + cumsum(trend_discount^(0:(data_length-1)))
    if(is.null(lag)==T){
      return(trend_discount)
    } else {
      return(trend_discount[[lag]])
    }
  } else {
    trend_discount <- data_length + cumsum(trend_discount^(0:(horizon-1)))
    return(trend_discount)
  }
}

get_time_weights <- function(dep_var, time_weight){
  if(length(time_weight)!=1) stop("Only one time weight value should be provided")
  time_weight^(length(1:length(dep_var))-(1:length(dep_var)))^2
}

make_reg_matrix <- function(fit_output, horizon){
  tmp_tibble <- tibble(.rows = horizon)
  for(i in 1:length(fit_output$model_summary$regressor_names)){
    tmp_tibble[fit_output$model_summary$regressor_names[i]] <- 0
  }
  tmp_tibble %>% 
    mutate(date = seq.Date(from = as.Date(fit_output$model_summary$max_date + months(1))
                           , length.out = horizon
                           , by = "month")
           , month = factor(months(date, abbr=TRUE), levels = month.abb)
           , trend = get_trend_discounts(data_length = fit_output$model_summary$train_size
                                         , trend_discount = fit_output$param$trend_discount
                                         , horizon = horizon)) %>% 
    select(month, trend, everything())
}

# Splits -------------------------------

# Main ts_split function

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

# ts split for glmnet & mlr (Map)

ts_split <- function(data, test_size, lag){
  ts_split_helper <- function(data, test_size, lag, iter){
    ts_len <- length(data[,1][[1]])
    train <- data[1:(ts_len-test_size-lag+iter),]
    test <- data[ts_len-test_size+iter,]
    list(train = train, test = test, pars = list(lag = lag, iter = iter))
  }
  map(1:test_size, ~ts_split_helper(data = data, test_size = test_size, lag = lag, iter = .x))
}

# mape -------------------------------

mape <- function(real, pred){
  return(round((abs(real-pred)/real),3))
}

#---
