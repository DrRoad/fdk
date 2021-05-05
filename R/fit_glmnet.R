#' Fit a Regularized Generalize Linear Model
#'
#' @param .data tibble/data.frame: matrix of response and covariates.
#' @param parameter list: Combination of parameter to estimate the model.
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
  
  stopifnot(is.numeric(parameter$glmnet$lambda) & length(parameter$glmnet$lambda)<2)
  
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
      if(length(parameter$glmnet$lambda) == 1) {
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
            glmnet::cv.glmnet(
              x = features_matrix
              , y = y_var
              , alpha = parameter$glmnet$alpha
              , nfolds = n_folds
              , foldid = fold_id
              , weights = time_weights_tmp
              , type.measure = parameter$glmnet$metric_lambda_optim
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
