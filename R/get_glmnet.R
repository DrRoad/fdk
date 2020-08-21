#' Fit a Regularized Generalize Linear Model
#'
#' @param .data DataFrame or tibble
#' @param y_var String. Column name of the time series to be forecasted.
#' @param date_var String.Column name of the time series to be forecasted.
#' @param parameter List. parametereters to be used for estimation. There are 4 parametereters: first,
#' *alpha* in the space [0,1] controls whether it is a Ridge (L2) a LASSO (L1) shrinkage method, respectively.
#' Any number that lies between is considered as ElasticNet regression, a combination of both regularizations.
#' The other 2 parametereters are time weights and trend discount.
#'
#' @import dplyr
#' @import fastDummies
#' @import glmnet
#' @author Obryan Poyser
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_glmnet()
#' }
get_glmnet <- function(.data, y_var, date_var, parameter, tune_model = FALSE) {
  if (is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- "y_var"
    date_var <- "date"
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  # For CV, tune passes a grid of values
  
  if(tune_model == FALSE){
    time_weight <- parameter[["glmnet"]][["time_weight"]]
    trend_discount <- parameter[["glmnet"]][["trend_discount"]]
    alpha <- parameter[["glmnet"]][["alpha"]]
    lambda <- parameter[["glmnet"]][["lambda"]]
  } else {
    time_weight <- parameter[["glmnet"]][["grid"]][["time_weight"]]
    trend_discount <- parameter[["glmnet"]][["grid"]][["trend_discount"]]
    alpha <- parameter[["glmnet"]][["grid"]][["alpha"]]
  }
  

  # Time weight vector

  time_weights_tmp <- get_time_weights(y_var = .data[[y_var]], time_weight = time_weight)
  factor_var <- setdiff(names(.data)[sapply(.data, function(x) ifelse(is.character(x) | is.factor(x), T, F))], na_exclude)
  x_excluded <- unique(c(na_exclude, parameter$glmnet$job$x_excluded))
  x_var <- names(.data)[!(names(.data) %in% x_excluded)]
  
  # Design matrix
  
  y_train <- .data[[y_var]]
  x_train <- .data %>% 
    select(setdiff(names(.), x_excluded)) %>% 
    fastDummies::dummy_cols(select_columns = factor_var
                            , remove_selected_columns = T
                            , remove_first_dummy = T) %>% 
    as.matrix()

  # Fit

  if (parameter[["glmnet"]][["job"]][["optim_lambda"]] == FALSE) {
    model_fit <- glmnet(
      x = x_train, y = y_train, weights = time_weights_tmp
      , alpha = alpha
      , lambda = lambda
    )
  } else {
    model_fit_tmp <- cv.glmnet(
      x = x_train, y = y_train,
      alpha = alpha,
      weights = time_weights_tmp,
      type.measure = "mae"
    )

    model_fit <- glmnet(
      x = x_train, y = y_train,
      weights = time_weights_tmp,
      alpha = alpha,
      lambda = model_fit_tmp$lambda.min
    )
  }

  .fit_output <- list(
    model = "glmnet"
    , model_fit = model_fit
    #, prescription = prescription
    , parameter = list(
        alpha = alpha
        , lambda = model_fit[["lambda"]]
        , time_weight = time_weight
        , trend_discount = trend_discount
        , fit_summary = list(
          train_size = length(.data[,1][[1]])
          , time_weight_values = time_weights_tmp
          , train_pred = as.vector(predict(model_fit, newx = x_train))
          , x_var_matrix = colnames(x_train)
          , x_var = x_var
          , factor_var = factor_var
          )
        )
    )
  attr(.fit_output, "prescription") <- prescription
  return(.fit_output)
}
