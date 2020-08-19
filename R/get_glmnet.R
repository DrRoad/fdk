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
get_glmnet <- function(.data, y_var, date_var, parameter) {
  if (is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- "y_var"
    date_var <- "date"
    frequency <- prescription$frequency
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }

  # Time weight vector

  time_weights_tmp <- get_time_weights(y_var = .data[[y_var]], time_weight = parameter[["glmnet"]][["time_weight"]])

  # Design matrix

  reg_excluded <- unique(c(na_exclude, parameter[["glmnet"]][["job"]][["reg_excluded"]]))

  # Bind trend and seasonal regressors, it can be created outside for efficiency
  
  .data <- get_regressors(.data = .data, date_var = date_var, frequency = frequency)

  regressor_names <- names(.data)[!(names(.data) %in% na_exclude)]

  if (is.null(parameter[["glmnet"]][["job"]][["reg_excluded"]]) == FALSE) {
    .data <- .data[, !(names(.data) %in% parameter[["glmnet"]][["job"]][["reg_excluded"]])]
  }

  factor_vars <- names(.data)[sapply(.data, function(x) ifelse(is.character(x) | is.factor(x), T, F))]

  ## .data frame to matrix

  y_train <- .data[[y_var]]
  x_train <- .data[, regressor_names] %>%
    # mutate_at(.vars = factor_vars, as.factor) %>%
    dummy_cols(
      select_columns = factor_vars, remove_first_dummy = T,
      remove_selected_columns = T
    ) %>%
    as.matrix()

  # Fit

  if (parameter[["glmnet"]][["job"]][["optim_lambda"]] == FALSE) {
    model_fit <- glmnet(
      x = x_train, y = y_train, weights = time_weights_tmp,
      alpha = parameter[["glmnet"]][["alpha"]],
      lambda = parameter[["glmnet"]][["lambda"]]
    )
  } else {
    model_fit_tmp <- cv.glmnet(
      x = x_train, y = y_train,
      alpha = parameter[["glmnet"]][["alpha"]],
      weights = time_weights_tmp,
      type.measure = "mae"
    )

    model_fit <- glmnet(
      x = x_train, y = y_train,
      weights = time_weights_tmp,
      alpha = parameter[["glmnet"]][["alpha"]],
      lambda = model_fit_tmp$lambda.min
    )
  }

  fit_output <- list(
    model = "glmnet",
    model_fit = model_fit,
    fit_summary = list(
      train_size = length(.data[, 1][[1]]),
      train_pred = as.vector(predict(model_fit, newx = x_train)),
      x_names = colnames(x_train),
      regressor_names = regressor_names,
      factor_vars = factor_vars
    ),
    prescription = prescription,
    parameter = list(
      alpha = parameter[["glmnet"]][["alpha"]],
      lambda = model_fit[["lambda"]],
      time_weights = time_weights_tmp,
      trend_discount = parameter[["glmnet"]][["trend_discount"]]
    )
  )
  return(fit_output)
}
