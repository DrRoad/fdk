
grid_mlr <- expand_grid(time_weight = seq(from = 0.7, to = 1, by = 0.05)
                    , trend_discount = seq(from = 0.8, to = 1, by = 0.05))

parameter <- list(glmnet = list(time_weight = 0.9, trend_discount = .9, alpha = 0, lambda = 0
                                , grid = grid
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.1
                                             , n_best_model = 1))
                  , glm = list(time_weight = 0.9, trend_discount = 0.9
                               , grid = grid_mlr
                               , job = list(x_excluded = NULL
                                            , n_best_model = 1)))


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

