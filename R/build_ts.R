
# build_ts

build_ts <- function(data, frequency = 12){
  if(class(data)[1] == "tbl_df" || class(data)[1] == "tbl" || class(data)[1] == "data.frame"){
    # Start date ---------------------
    start_date <- as.Date(min(data$date))
    # Temporal adjustements ---------------------
    if(frequency == 12){
      temporal <- "month"
      factor_gen <- function(x) month(x)
      unit_start <- month(start_date)
      year_start <- year(start_date)
    }
    if(frequency == 52){
      temporal <- "week"
      factor_gen <- function(x) week(x)
      unit_start <- week(start_date)
      year_start <- year(start_date)
    }
    if(frequency == 365){
      temporal <- "day"
      factor_gen <- function(x) day(x)
      unit_start <- day(start_date)
      year_start <- year(start_date)
    }
    # Get dates ---------------------
    vector_start <- c(year_start,unit_start)
    # Create Time Series ---------------------
    ts <- ts(round(data$y,3), frequency = frequency, start = vector_start)
    # Build tibble ---------------------
    tmp <- tibble(date = seq.Date(from = start_date, by = temporal, length.out = nrow(data)),
                  y = round(data$y,3), reg = data$reg)
    tmp <- tmp %>%
      mutate(date = date, time_seas = as.factor(factor_gen(date))) %>%
      as_tsibble(index = date)
    tmp$date <- as.Date(tmp$date, format = "%Y-%m-%d")
    # Give temporal attribute to objects ---------------------
    attr(ts, "frequency") <- temporal
    attr(tmp, "frequency") <- temporal
    return(list(ts,tmp))
  }else if(class(data) == "list"){
    # Start date ---------------------
    start_date <- as.Date(min(data$date))
    # Temporal adjustements ---------------------
    if(frequency == 12){
      temporal <- "month"
      factor_gen <- function(x) month(x)
      unit_start <- month(start_date)
      year_start <- year(start_date)
    }
    if(frequency == 52){
      temporal <- "week"
      factor_gen <- function(x) week(x)
      unit_start <- week(start_date)
      year_start <- year(start_date)
    }
    if(frequency == 365){
      temporal <- "day"
      factor_gen <- function(x) day(x)
      unit_start <- day(start_date)
      year_start <- year(start_date)
    }
    # Get dates ---------------------
    vector_start <- c(year_start,unit_start)
    # Create Time Series ---------------------
    ts <- ts(round(data$y,3), frequency = frequency, start = vector_start)
    # Build tibble ---------------------
    tmp <- tibble(date = seq.Date(from = start_date, by = temporal, length.out = length(data$y)),
                  y = round(data$y,3), reg = rep(0,length(data$y)))
    tmp <- tmp %>%
      mutate(date = date, time_seas = as.factor(factor_gen(date))) %>%
      as_tsibble(index = date) 
    tmp$date <- as.Date(tmp$date, format = "%Y-%m-%d")
    # Give temporal attribute to objects ---------------------
    attr(ts, "frequency") <- temporal
    attr(tmp, "frequency") <- temporal
    return(list(ts,tmp))
  }
}

#---
