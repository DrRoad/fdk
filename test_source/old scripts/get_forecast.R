#' Generate forecast figures
#'
#' @param .fit_output Class ".fit_output".
#' @param x_data Data frame. Design matrix to calculate predicted/forecast figures.
#' @param horizon Numeric. Number of periods ahead to forecast.
#' @param tune Logical. Results will be used to define the best model.
#'
#' @import dplyr
#' @import stats
#' @import stringr
#' @return data-frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_forecast()
#' }
get_forecast <- function(.fit_output, x_data = NULL, tune = FALSE, horizon) {
  if(is.null(attributes(.fit_output)[["prescription"]]) == FALSE) {
    prescription <- attributes(.fit_output)[["prescription"]]
    # y_var <- prescription$y_var
    # date_var <- prescription$date_var
    # freq <- prescription$freq
    # na_exclude <- unique(c(prescription$key, y_var, date_var))
    if (prescription$freq == 12) {
      freq_string <- "months"
    }
  }
  if (.fit_output[["model"]] == "arima") {
    if (tune == TRUE) {
      # message("here")
      x_data %>%
        transmute(
          y_var_true = y_var
          , y_var_fcst = last(as.numeric(forecast(.fit_output[["model_fit"]], prescription[["lag"]])[["mean"]]))
          , as_tibble(.fit_output[["parameter"]])
        )
    } else {
      # message("here 1")
      tibble(
        date = seq.Date(from = (prescription$max_date + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(forecast(.fit_output[["model_fit"]], horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  } else if (.fit_output[["model"]] == "glmnet") {
    x_data_int <- make_reg_matrix(.fit_output = .fit_output, x_data = x_data, horizon = horizon)
    if (is.null(x_data) == TRUE) {
      # Synthetic x_data
      predict.glmnet(object = .fit_output[["model_fit"]], newx = x_data_int) %>%
        as.vector() %>%
        enframe(name = "date", value = "y_var_fcst") %>%
        mutate(
          date = seq.Date(
            from = (prescription$max_date + months(1)),
            by = freq_string,
            length.out = horizon
          ),
          model = "glmnet"
        )
    } else if (tune == TRUE) {
      # yvar_pred = as.numeric(predict.glmnet(object = .fit_output$model_fit, newx = x_data_int))
      x_data_tmp <- x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = as.numeric(predict.glmnet(object = .fit_output[["model_fit"]], newx = x_data_int)),
          trend_discount = .fit_output$parameter$trend_discount,
          time_weight = .fit_output$parameter$time_weight,
          alpha = .fit_output$parameter$alpha,
          lambda = .fit_output$parameter$lambda
        )
      return(x_data_tmp)
    }
  } else if (.fit_output[["model"]]== "glm") {
    x_data_int <- make_reg_matrix(.fit_output = .fit_output, x_data = x_data, horizon = horizon)
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = as.numeric(predict(object = .fit_output[["model_fit"]], newdata = x_data_int)),
          trend_discount = .fit_output$parameter$trend_discount,
          time_weight = .fit_output$parameter$time_weight
        )
    } else if (is.null(x_data) == TRUE) {
      # Synthetic x_data
      predict.glm(object = .fit_output[["model_fit"]], newdata = x_data_int) %>%
        as.vector() %>%
        enframe(name = "date", value = "y_var_fcst") %>%
        mutate(
          date = seq.Date(
            from = (prescription$max_date + months(1)),
            by = freq_string,
            length.out = horizon
          ),
          model = "glm"
        )
      }
  } else if (.fit_output[["model"]] == "croston") { # missing tune
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = as.numeric(.fit_output$model_fit[["mean"]])[attributes(x_data)[["prescription"]][["lag"]]], 
          parameter = list(NULL)
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(croston(.fit_output$y_var_int, horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  } else if(.fit_output[["model"]] == "ets"){
    suppressWarnings({
      if (tune == TRUE) {
        x_data %>%
          transmute(
            y_var_true = y_var,
            y_var_fcst = last(as.numeric(predict(.fit_output[["model_fit"]], attributes(x_data)[["prescription"]][["lag"]])[["mean"]])),
            parameter = str_remove_all(.fit_output[["model_fit"]][["method"]], "ETS|NNAR|,|\\)|\\(")
          )
      } else {
        tibble(
          date = seq.Date(from = (prescription$max_date + months(1)), length.out = horizon, by = "months"),
          y_var_fcst = as.numeric(predict(.fit_output[["model_fit"]], horizon)[["mean"]]),
          model = .fit_output[["model"]]
        )
      }
    }
    )
  } else if(.fit_output[["model"]] == "neural_network"){
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(as.numeric(predict(.fit_output[["model_fit"]], attributes(x_data)[["prescription"]][["lag"]])[["mean"]])),
          parameter = str_remove_all(.fit_output[["model_fit"]][["method"]], "ETS|NNAR|,|\\)|\\(")
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription$max_date + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(predict(.fit_output[["model_fit"]], horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  } else if(.fit_output[["model"]] == "tbats"){
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(as.numeric(predict(.fit_output[["model_fit"]], attributes(x_data)[["prescription"]][["lag"]])[["mean"]])),
          parameter = list(.fit_output[["model_fit"]][["parameters"]][["vect"]])
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription$max_date + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(predict(.fit_output[["model_fit"]], horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  } else if(.fit_output[["model"]] == "seasonal_naive"){
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = as.numeric(.fit_output$model_fit[["mean"]])[attributes(x_data)[["prescription"]][["lag"]]], 
          parameter = list(.fit_output[["model_fit"]][["parameters"]][["vect"]])
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(snaive(.fit_output$y_var_int, horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  } else if(.fit_output[["model"]] == "dyn_theta"){
    if (tune == TRUE) {
      if(nrow(x_data) > 12){
        x_data %>%
          transmute(
            y_var_true = y_var,
            y_var_fcst = last(as.numeric(dotm(.fit_output$y_var_int, attributes(x_data)[["prescription"]][["lag"]])[["mean"]])), 
            parameter = list(NULL)
          )
      }else if(nrow(x_data) <= 12){
        x_data %>%
          transmute(
            y_var_true = y_var,
            y_var_fcst = last(as.numeric(dotm(.fit_output$y_var_int, attributes(x_data)[["prescription"]][["lag"]], s = FALSE)[["mean"]])), 
            parameter = list(NULL)
          )
      }
    } else {
      if(length(.fit_output$y_var_int) > 12){
        tibble(
          date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
          y_var_fcst = as.numeric(dotm(.fit_output$y_var_int, horizon)[["mean"]]),
          model = .fit_output[["model"]]
        )
      }else if(length(.fit_output$y_var_int) <= 12){
        tibble(
          date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
          y_var_fcst = as.numeric(dotm(.fit_output$y_var_int, horizon, s = FALSE)[["mean"]]),
          model = .fit_output[["model"]] 
        )
      }
    }
  } else if(.fit_output[["model"]] == "tslm"){
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(as.numeric(forecast(.fit_output$y_var_int, attributes(x_data)[["prescription"]][["lag"]])[["mean"]])),
          parameter = list(NULL)
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(forecast(.fit_output$model_fit, h = horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  } else if(.fit_output[["model"]] == "prophet"){
    if (tune == TRUE) {
      # Main df
      future <- make_future_dataframe(.fit_output$model_fit, 
                                      periods = attributes(x_data)[["prescription"]][["lag"]],
                                      freq = "months")
      # Output
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(predict(.fit_output$model_fit, future)$yhat),
          parameter = list(NULL)
        )
    } else {
      # Main df
      future <- make_future_dataframe(.fit_output$model_fit, 
                                      periods = horizon,
                                      freq = "months")
      # Output
      tibble(
        date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = tail(predict(.fit_output$model_fit, future)$yhat,horizon),
        model = .fit_output[["model"]]
      )
    }
  } else if(.fit_output[["model"]] == "svm"){
    if (tune == TRUE) { # tuning mode
      # Main df
      last_trend <- length(.fit_output$y_var_pred) # latest trend from fit
      trend_stamp <- c((last_trend + 1):last_trend + 13) # get 12 months
      # Dampening
      trend.dampening <- (0.99)^(0:(length(trend_stamp)-1))
      dampened_trend <- trend_stamp*trend.dampening
      dates_stamp <- seq.Date(from = (max(x_data$date_var))-months(3), length.out = 12, by = freq_string) # get 12 months
      new_data <- data.frame(trend = dampened_trend, seasonal_var = months(dates_stamp)) # new data
      # Params
      svm_param <- paste0("Cost:",round(.fit_output$model_fit$cost,2),"; ",
                          "Gamma:",round(.fit_output$model_fit$gamma,2),"; ",
                          "Epsilon:",round(.fit_output$model_fit$epsilon,2))
      # Output
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = predict(.fit_output$model_fit, new_data)[attributes(x_data)[["prescription"]][["lag"]]], # Get lag 4
          parameter = svm_param
        )
    } else { # forecast mode
      # Main df
      last_trend <- length(.fit_output$y_var_pred) # latest trend from fit
      trend_stamp <- c((last_trend + 1):(last_trend + horizon))# trend for fcst
      # Dampening
      trend.dampening <- (0.99)^(0:(length(trend_stamp)-1))
      dampened_trend <- trend_stamp*trend.dampening
      dates_stamp <- seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = freq_string)
      new_data <- data.frame(trend = dampened_trend, seasonal_var = months(dates_stamp))
      # Params
      svm_param <- paste0("Cost:",round(.fit_output$model_fit$cost,2),"; ",
                          "Gamma:",round(.fit_output$model_fit$gamma,2),"; ",
                          "Epsilon:",round(.fit_output$model_fit$epsilon,2))
      # Output
      tibble(
        date = dates_stamp,
        y_var_fcst = tail(predict(.fit_output$model_fit, new_data), horizon),
        model = .fit_output[["model"]]
      )
    }
  }
}
