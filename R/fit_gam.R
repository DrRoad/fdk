# GAM --------------------------------------------------------

#' Fit Generalized Additive Model
#'
#' @param .data tibble/data.frame:  
#' @param parameter list: hyperparameter and parameter to be applied.
#'
#' @return
#' @importFrom stringr str_remove_all
#' @import mgcv
#' @export
#'
#' @examples
fit_gam <- function(.data, parameter = list()){

  key <- attributes(.data)[["key"]]
  features <- setdiff(names(.data), c("date_var", "y_var"))
  features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(.data[[x]])))]
  features_factor <- setdiff(features, features_cont)
  
  # Time weight vector ---------------------------------------------------------
  
  if(is.null(parameter$gam$time_weight) | parameter$gam$time_weight == 1){
    time_weights_tmp <- rep(1, nrow(.data))
  } else {
    time_weights_tmp <- get_time_weights(y_var = .data$y_var
                                         , time_weight = parameter$gam$time_weight)
  }
  
  # If there are not enough data points perform a simpler model
  if(.log[[key]]$dates_check$n_dates < 13){
    gam_formula <- as.formula("y_var ~ trend")
  } else {
    # Replace formula ---------------------------------------------------------
    if(length(parameter$gam$formula) != 0){
      gam_formula <- as.formula(parameter$gam$formula)
    } else {
      # Smoothed features -------------------------------------------------------
      if(any(is.null(names(parameter$gam$smoothed_features)))){
        sf_formula = ""
      } else {
        sf_formula <- plyr::ldply(parameter$gam$smoothed_features, unlist) %>% 
          transmute(sf = paste0("s(", .id, ", k = ", k, ", bs = '", bs, "')")) %>% 
          pull() %>% 
          str_remove_all(pattern = "k = NA, ")
      }
      
      # Linear features ---------------------------------------------------------
      
      lf <- setdiff(features, c(names(parameter$gam$smoothed_features)
                                , parameter$gam$excluded_features))
      if(length(lf) > 0){
        lf_formula <- lf
      } else {
        lf_formula <- ""
      }
      
      # Formula -----------------------------------------------------------------
      
      formula_tmp <- paste0(c("y_var ~ 1"
               , sf_formula
               , lf_formula)
             , collapse = " + ") %>% 
        str_remove_all(pattern = " \\+ $")
      
      gam_formula <- as.formula(formula_tmp)
    }
  }
  
  # Fitting -----------------------------------------------------------------
  
  tryCatch(
    {
      gam(formula = gam_formula
          , family = parameter$gam$link_function
          #, weights = time_weights_tmp
          , data = .data
          , method = "REML")
    }
    , error = function(err){
      message("Too many parameters, fitting a smooth trend model.")
      gam(formula = as.formula('y_var ~ s(trend, bs = "tp")')
          , family = parameter$gam$link_function
          #, weights = time_weights_tmp
          , data = .data
          , method = "GCV.Cp")
    }
  )
}
