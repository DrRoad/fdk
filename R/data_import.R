

#' Importing data from local and shared sources
#'
#' @param source_conf list: configuration for data source.
#'
#' @importFrom dplyr filter
#' @import tidyverse
#' @return
#' @export
#'
#' @examples
import_data <- function(source_conf){
 
  if(source_conf$source == "oc"){
    
    stopifnot(any(unlist(source_conf$db) == "full_sales"))
    
    oc_data_init <- get_oc_data(db = source_conf$db
                           , countries = source_conf$countries
                           , gbus = source_conf$gbus
                           , date_cycle = source_conf$date_cycle)
    
    # Filters
    
    sales_category <- if(is.null(source_conf$filters$category)==F){
      source_conf$filters$category
    } else {
      "HistoricalSales"
    }
    sales_cycle_category <- if(is.null(source_conf$filters$category)==F){
      source_conf$filters$cycle_category
    } else {
      "before_cleansing"
    }
    
    # Sales and filters
    
    sales_tmp <- oc_data_init$full_sales %>%
      filter(category == sales_category
             , cycle_category == sales_cycle_category) %>% 
      dplyr::select(forecast_item, date, sales = m_sales)
    
    # if forecast in list
    
    if(any(source_conf$db == "full_forecast")){
      forecast_tmp <- oc_data_init$full_forecast %>% 
        filter(cycle_category == sales_cycle_category) %>% 
        dplyr::select(forecast_item, date, sales = forecast)
      
      if(source_conf$join_hist_forecast == T){
        sales_tmp <- sales_tmp %>% 
          mutate(series_type = "history") %>% 
          bind_rows(
            forecast_tmp
          )
        
        is_hist_reg <- sales_tmp %>% 
          rename(reg_name = series_type, reg_date = date) %>% 
          dplyr::select(-sales) %>% 
          mutate(reg_value = case_when(
            reg_name == "history" ~ 1
            , TRUE ~ 0
          ))
        
        sales_tmp <- sales_tmp %>% 
          dplyr::select(-series_type)
      }
    }
    
    # if regressors in list
    
    if(any(source_conf$db == "regressor")){
      
      if(any(source_conf$db == "full_forecast")){
        regressor <- oc_data_init$regressor %>% 
          bind_rows(is_hist_reg)
      } else {
        regressor <- oc_data_init$regressor
      }
      
      sales_tmp <- sales_tmp %>% 
        left_join(regressor, by = c("forecast_item", "date" = "reg_date")) %>%
        replace_na(replace = list(series_type = "forecast", reg_value = 0)) #%>% 
        #mutate(series_type = factor(series_type, c("history", "forecast")))
    }
    
    sales_tmp <- sales_tmp %>% 
      filter(!is.na(date))
    
    list(sales = sales_tmp)
  }
}


#' Get Operational Cycle data
#'
#' @param db list: list of DB from the OC.
#' @param countries string: vector of country codes.
#' @param gbus string: GBU to be extracted.
#' @param date_cycle date: date of the cycle in yyyy-mm-dd format.
#'
#' @import tidyverse
#' @import stringr
#' @importFrom purrr map
#' @importFrom purrr map2
#' @return
#' @export
#'
#' @examples
get_oc_data <- function(db = list(), countries, gbus, date_cycle){
  
  root_input <- "//E21flsbcnschub/BCN_SC_HUB/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/"
  admitted <- c("full_sales", "full_forecast", "forecast_item_info", "regressor")
  
  if(!any(unlist(db) %in% admitted)){
    stop(paste0("Only the following DB's are admitted: ", paste0(admitted, collapse = ", ")))
  }
  
  imported_oc <- readRDS("data/loc_mapping.rds") %>% 
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
    ungroup() %>% 
    expand_grid(db = unlist(db)) %>% 
    mutate(path_input = paste0(path_input, "RData/",db, ".RData")) %>% 
    dplyr::select(path_input, db) %>% 
    mutate(data = map(path_input, ~rdata2obj(.x))) %>% 
    dplyr::select(-path_input) %>% 
    group_nest(db) %>% 
    mutate(data = map(data, ~.x %>% unnest(data)))
  
  
  names(imported_oc$data) <- imported_oc$db
  imported_oc <- imported_oc$data
  
  if("regressor" %in% names(imported_oc)){
    imported_oc[["regressor"]] <- imported_oc[["regressor"]] %>% 
      dplyr::select(forecast_item, reg_name, reg_date, reg_value) %>% 
      mutate(reg_name = str_remove(string = reg_name
                                   , pattern = paste0(forecast_item, " - |-"))
      ) %>% 
      dplyr::distinct()
  }
  imported_oc
}
