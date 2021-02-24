#' Fit a Regularized Generalize Linear Model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param date_var String.Column name of the time series to be forecasted.
#' @param parameter List. Parameters to be used for estimation. There are 4 parametereters: first,
#' *alpha* in the space [0,1] controls whether it is a Ridge (L2) a LASSO (L1) shrinkage method, respectively.
#' Any number that lies between is considered as ElasticNet regression, a combination of both regularizations.
#' The other 2 parametereters are time weights and trend discount.
#'
#' @import dplyr
#' @import fastDummies
#' @importFrom glmnet glmnet
#' @author Obryan Poyser
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_glmnet()
#' }
fit_glmnet <- function(.data, parameter){
  
  key_int <- attributes(.data)[["key"]]
  features <- setdiff(names(.data), c("date_var", "y_var", unlist(parameter$glmnet$excluded_features)))
  features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(.data[[x]])))]
  features_factor <- setdiff(features, c(features_cont))
  

  if(is.null(parameter$glmnet$time_weight) | parameter$glmnet$time_weight == 1){
    time_weights_tmp <- rep(1, nrow(.data))
  } else {
    time_weights_tmp <- get_time_weights(y_var = .data$y_var
                                         , time_weight = parameter$glmnet$time_weight)
  }
    
  y_var <- as.numeric(.data[["y_var"]])
  features_matrix <- .data %>%
    dplyr::select(all_of(features)) %>% 
    fastDummies::dummy_cols(select_columns = features_factor
                            , remove_selected_columns = T
                            , remove_first_dummy = T) %>% 
    as.matrix()

  fit <- tryCatch(
    {
      if(is.null(parameter$glmnet$lambda) == FALSE) {
        fit <- glmnet(
          x = features_matrix, y = y_var, weights = time_weights_tmp
          , alpha = parameter$glmnet$alpha
          , lambda = parameter$glmnet$lambda
        )
      } else {
        set.seed(parameter$glmnet$seed)
        n_folds <- 3
        fold_id <- sample(rep(seq(n_folds), length.out = nrow(.data)))
        
        lambda_min <- tryCatch(
          {
            cv.glmnet(
              x = features_matrix
              , y = y_var
              , alpha = parameter$glmnet$alpha
              , nfolds = n_folds
              , foldid = fold_id
              , weights = time_weights_tmp
              , type.measure = parameter$glmnet$lambda_measure
            )[["lambda.min"]]
          }
          , error = function(err) 1
        )
        
        
        glmnet(
          x = features_matrix, y = y_var
          , weights = time_weights_tmp
          , alpha = parameter$glmnet$alpha
          , lambda = lambda_min
          , family = parameter$glmnet$link_function
        )
      }
    }
    , error = function(err){
      message("Error in GLMNET model")
    }
  )
  
  return(fit)

}
