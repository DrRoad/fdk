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
  
  # Attributes --------------------------------------------------------------
  
  assign("af_log", value = tibble(key = character(), log_time = Sys.time(), module = NA_character_
                  , log = list(), .rows = 1) %>% slice(0)
         , envir = .GlobalEnv)
  
  attr(.data_init[["date_var"]], "date_meta") <- c(date_format, freq, freq_name
                                                 , as.character(range(.data_init$date_var)))
  
  update_logger(key = "all_keys"
                , module = "prescription"
                , new_log = tibble(date_format = date_format
                                   , freq = freq
                                   , freq_name = freq_name
                                   , date_min = as.character(min(.data_init$date_var))
                                   , date_max = as.character(max(.data_init$date_var))))

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
update_logger <- function(key = character(), module = character(), new_log = list(), env = globalenv()){
  stopifnot(is.character(module))
  
  entering_log <- bind_rows(tibble(key = ifelse(is.null(key), NA_character_, key)
                    , log_time = Sys.time()
                    , module = module
                    , log = list(new_log)))
  
  if(module %in% c("prescribe", "prescription")){
    assign("af_log", value = tibble(key = character(), log_time = Sys.time(), module = NA_character_
                    , log = list(), .rows = 1) %>% slice(0)
           , envir = .GlobalEnv)
    assign("af_log", value = entering_log, envir = .GlobalEnv)
  } else {
    entering_log
  }
  #assign("logger", value = bind_rows(logger, new_line), envir = env)
}

task_transfer <- function(.output){
  if(is.list(.output)){
    .output[[1]]
  } else {
    .output
  }
}

