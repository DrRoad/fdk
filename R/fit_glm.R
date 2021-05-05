#' Fit a Generalized Linear Model
#'
#' @param .data tibble/data.frame: matrix of response and covariates.
#' @param parameter list: Combination of parameter to estimate the model.
#'
#' @return data-frame
#' @import stats
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' get_glm()
#' }
fit_glm <- function(.data, parameter) {
  
  key <- attributes(.data)[["key"]]
  features <- setdiff(names(.data), c("date_var", "y_var"))
  features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(.data[[x]])))]
  features_factor <- setdiff(features, features_cont)
  
  # Time weight vector ---------------------------------------------------------
  
  if(is.null(parameter$glm$time_weight) | parameter$glm$time_weight == 1){
    time_weights_tmp <- rep(1, nrow(.data))
  } else {
    time_weights_tmp <- get_time_weights(y_var = .data$y_var
                                         , time_weight = parameter$glm$time_weight)
  }
  
  # If there are not enough data points perform a simpler model
  if(.log[[key]]$dates_check$n_dates < 13){
    glm_formula <- as.formula("y_var ~ trend")
  } else {
    # Replace formula ---------------------------------------------------------
    if(length(parameter$glm$formula) != 0){
      glm_formula <- parameter$glm$formula
    } else {
      # Linear features ---------------------------------------------------------
      
      lf <- setdiff(features, c(parameter$glm$excluded_features))
      if(length(lf) > 0){
        lf_formula <- lf
      } else {
        lf_formula <- ""
      }
      
      # Formula -----------------------------------------------------------------
      
      glm_formula <- as.formula(paste0(c("y_var ~ 1"
                                         , lf_formula)
                                       , collapse = " + "))
    }
  }
  
  # Fitting -----------------------------------------------------------------
  
  tryCatch(
    {
      glm(formula = glm_formula
          , family = parameter$glm$link_function
          , weights = time_weights_tmp
          , data = .data)
    }
    , error = function(err){
      message("Too many parameters, fitting a trend model.")
      glm(formula = as.formula('y_var ~ trend')
          , family = parameter$glm$link_function
          , weights = time_weights_tmp
          , data = .data)
    }
  )
}

