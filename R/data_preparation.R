#' Data prescription
#' 
#' Prescribe provides a simple way to prepare the data for ingestion
#'
#' @param .data DataFrame or tibble.
#' @param key Column name of the key if any.
#' @param y_var Column name of the variable to be forecasted.
#' @param date_var Column name of time index.
#' @param freq Frequency of the data.
#' @param reg_name Column name of the regressors.
#' @param reg_value Column name of the regressors' values.
#'
#' @return data-frame
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' prescribe_ts()
#' }
prescribe_ts <- function(.data_init, key = NULL, y_var
                         , date_var, reg_name = NULL
                         , reg_value = NULL, freq, date_format){
  
  # Validators --------------------------------------------------------------
  
  stopifnot(freq %in% c(1, 12, 4, 52, 365))
  stopifnot(date_format %in% c("ymd", "dmy", "ym"))
  
  freq_name <- data.frame(freq_number = c(1, 4, 12, 52, 365)
             , freq_name = c("year", "quarter", "month", "week", "day")) %>% 
    dplyr::filter(freq_number == freq) %>% 
    pull(freq_name)
  
  if(is.null(key)==TRUE){
    if(any(duplicated(.data_init[[date_var]])) == TRUE){
      stop("A duplicated date index has been found and no key column has been defined, please review the input data.")
    } else {
      .data_init <- .data_init %>% 
        mutate(key = "key_001")
    }
  }
  
  if(is.null(reg_name) == T | is.null(reg_value) == T){
    .data_init <- .data_init %>% 
      mutate(reg_name = NA_character_, reg_value = 0) %>% 
      rename("key" = key, "y_var" = y_var, "date_var" = date_var)
  } else {
    .data_init <- .data_init %>% 
      rename("reg_name" = reg_name, "reg_value" = reg_value) %>% 
      rename("key" = key, "y_var" = y_var, "date_var" = date_var)
  }
  

  # Log init ----------------------------------------------------------------

  keys <- unique(.data_init$key)
  .log_init <- list()
  for(i in seq_along(keys)){
    .log_init[[i]] <- list(NULL)
  }
  
  prescription_log <- list(date_format = date_format
       , freq = freq
       , freq_name = freq_name
       , date_min = as.character(min(.data_init$date_var))
       , date_max = as.character(max(.data_init$date_var)))
  
  .log_init <- append(.log_init, values = list(prescription_log), after = 0)
  names(.log_init) <- c("prescription", keys)
  assign(x = ".log_init", value = .log_init, envir = .GlobalEnv)
  assign(x = ".log", value = .log_init, envir = .GlobalEnv)
  
  # Ouput -------------------------------------------------------------------

  .data_init %>% 
    dplyr::select(all_of(c("key", "date_var", "y_var")), everything()) %>% 
    mutate(across(.cols = c("reg_value"), .fns = ~if_else(is.na(.x), 0, .x))) %>%
    arrange(key, date_var) %>%
    group_nest(key) %>% 
    mutate(data = map2(data, key, ~{
      .x %>%
        structure(key = .y)
    }))
}



#' Logger updater
#'
#' @param key 
#' @param module 
#' @param new_log 
#' @param env 
#'
#' @return
#' @export
#'
#' @examples
log_update <- function(module = character(), key = character(), new_log = list()){
  stopifnot(is.character(module))
  if(exists(".log", envir = .GlobalEnv) == FALSE){
    .log <- .log_init
  }
  in_log_update <- list(append(new_log, values = list(sys_time = Sys.time())))
  names(in_log_update) <- module
  
  if(module %in% names(.log[[key]])){
    .log[[key]] <- modifyList(x = .log[[key]], val = in_log_update)
  } else {
    .log[[key]] <- append(.log[[key]], values = in_log_update)
    if(is.null(.log[[key]][[1]])){.log[[key]][[1]] <- NULL}
  }
  
  assign(x = ".log", value = .log, envir = .GlobalEnv)
  
  # if(module %in% c("validation", "validate")){
  #   key_init <- list(NULL)
  #   names(key_init) <- key
  #   key_init[[key]] <- in_log
  # } else {
  #   
  #   key_init <- list(append(.log[[key]], values = in_log_update))
  #   names(key_init) <- key
  # }
  
  # if(exists("key_init", envir = .GlobalEnv) == FALSE){
  # 
  #   in_log <- list(append(new_log, values = list(sys_time = Sys.time())))
  #   names(in_log) <- module
  #   key_init[[key]] <- in_log
  # } else if((module %in% names(key_init[[key]])) == FALSE){
  #   in_log_update <- list(append(new_log, values = list(sys_time = Sys.time())))
  #   names(in_log_update) <- module
  #   key_init <- list(append(key_init[[key]], values = in_log_update))
  #   names(key_init) <- key
  # } else {
  #   message("last")
  # }
  
  #assign(x = ".log", value = key_init, envir = .GlobalEnv)
}


# if(module %in% c("prescribe", "prescription")){
#   assign(".log_init", value = in_log, envir = env)
# } else {
#   if(exists(".log") == T){
#     if((module %in% names(.log)) == T){
#       modifyList(x = .log, val = in_log)
#     } else {
#       assign(x = ".log", value = append(x = .log, values = in_log), envir = env)
#     }
#   } else {
#     assign(x = ".log", value = append(x = .log_init, values = in_log), envir = env)
#   }
# }



