#' Fit a Generalized Linear Model
#'
#' @param .data Data frame or tibble with a response variable.
#' @param y_var String. Column name of the time series to be forecasted.
#' @param date_var String.Column name of the time series to be forecasted.
#' @param parameter List. Trend discount and time weight parameters.
#'
#' @return
#' @export
#'
#' @examples
get_glm <- function(.data, y_var, date_var, parameter) {
  if (is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- "y_var"
    date_var <- "date"
    freq <- prescription$freq
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  # Time weight vector
  
  time_weights_tmp <- get_time_weights(y_var = .data[[y_var]], time_weight = parameter$glm$time_weight)
  factor_var <- setdiff(names(.data)[sapply(.data, function(x) ifelse(is.character(x) | is.factor(x), T, F))], na_exclude)
  x_excluded <- unique(c(na_exclude, parameter$glm$job$x_excluded))
  x_var <- names(.data)[!(names(.data) %in% x_excluded)]
  
  # Design matrix
  
  # Fit
  
  model_fit <- glm(formula = as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
      , data = .data, weights = time_weights_tmp)
  
  # Output
  
  .fit_output <- list(
    model = "glm"
    , model_fit = model_fit
    #, prescription = prescription
    , parameter = list(
      time_weight = parameter$glm$time_weight
      , trend_discount = parameter$glm$trend_discount
      , fit_summary = list(
        data_size = length(.data[,1][[1]])
        , time_weight_values = time_weights_tmp
        , y_var_pred = as.vector(predict(model_fit, newx = .data))
        , x_var = x_var
        , factor_var = factor_var
      )
    )
  )
  attr(.fit_output, "prescription") <- prescription
  return(.fit_output)
}

