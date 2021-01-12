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
prescribe_ts <- function(.data, key = NULL, y_var, date_var, reg_name = NULL, reg_value = NULL, freq){
  # basic configuration
  attr(.data, "prescription") <- list(key = "key", y_var = "y_var"
                                      , date_var = "date_var", freq = "freq"
                                      , max_date = as.Date("1970-01-01"))
  if(is.null(key)==TRUE){
    if(any(duplicated(.data[[date_var]])) == TRUE){
      stop("A duplicated date index has been found and no key column has been defined, please review the input data.")
    } else {
      .data <- .data %>% 
        mutate(key = "key_001")
    }
  }
  if(is.null(reg_name) == T | is.null(reg_value) == T){
    .data_tmp <- .data %>% 
      mutate(reg_name = NA_character_, reg_value = 0)
  } else {
    .data_tmp <- .data %>% 
      rename("reg_name" = reg_name, "reg_value" = reg_value)
    attr(.data_tmp, "prescription")[["reg_name"]] <- "reg_name"
    attr(.data_tmp, "prescription")[["reg_value"]] <- "reg_value"
    attr(.data_tmp, "prescription")[["has_regressors"]] <- TRUE
    prescription <- attributes(.data_tmp)[["prescription"]]
  }
  .data_tmp <- .data_tmp %>% 
    rename("key" = key, "y_var" = y_var, "date_var" = date_var) %>% 
    dplyr::select(key, date_var, y_var, everything())
  attr(.data_tmp, "prescription")[["y_var"]] <- "y_var"
  attr(.data_tmp, "prescription")[["freq"]] <- freq
  attr(.data_tmp, "prescription")[["date_var"]] <- "date_var"
  attr(.data_tmp, "prescription")[["key"]] <- "key"
  attr(.data_tmp, "prescription")[["max_date"]] <- max(.data_tmp[["date_var"]])
  attr(.data_tmp, "prescription")[["multiple_keys"]] <- n_distinct(.data_tmp[["key"]])>1
  prescription <- attributes(.data_tmp)[["prescription"]]
  return(.data_tmp)
}

#---
