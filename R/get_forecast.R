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
#' @return
#' @export
#'
#' @examples
get_forecast <- function(.fit_output, x_data = NULL, horizon = NULL, tune = FALSE) {
  
  
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    #y_var <- prescription$y_var
    #date_var <- prescription$date_var
    #freq <- prescription$freq
    #na_exclude <- unique(c(prescription$key, y_var, date_var))
    
    if (prescription$freq == 12) {
      freq_string <- "month"
    }
  }

  if (.fit_output[["model"]] == "arima") {
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = .data[["y_var"]],
          y_var_fcst = last(as.numeric(forecast(
            .fit_output[["model_fit"]],
            attributes(x_data)[["prescription"]][["lag"]]
          )[["mean"]])),
          as_tibble(.fit_output[["parameter"]])
        )
    } else {
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
    
    # Synthetic x_data
    x_data_int <- make_reg_matrix(.fit_output = .fit_output, x_data = x_data, horizon = horizon)
    
    if (is.null(x_data) == TRUE) {
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
    } else if (tune == TRUE) {
      x_data_tmp <- x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = as.numeric(predict.glm(object = .fit_output[["model_fit"]], newdata = x_data_int)),
          trend_discount = .fit_output$parameter$trend_discount,
          time_weight = .fit_output$parameter$time_weight
        )
      }
  } else if (.fit_output[["model"]] == "croston") { # missing tune
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(as.numeric(croston(.fit_output$y_var_int, attributes(x_data)[["prescription"]][["lag"]])[["mean"]]))
          , parameter = list(.fit_output[["model_fit"]][["parameters"]][["vect"]])
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(croston(.fit_output$y_var_int, horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
    
    if(tune == TRUE){
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(as.numeric(predict(.fit_output[["model_fit"]], attributes(x_data)[["prescription"]][["lag"]])[["mean"]]))
          , parameter = str_remove_all(.fit_output[["model_fit"]][["method"]], "ETS|NNAR|,|\\)|\\(")
        )
    } else {
      get_croston(.data, parameter = parameter, horizon = horizon) %>% 
        .[["y_var_fcst"]] %>% 
        enframe(name = "date", value = "y_var_fcst") %>% 
        mutate(
          date = seq.Date(
            from = (prescription$max_date + months(1)),
            by = freq_string,
            length.out = horizon
          ),
          model = "croston"
        )
    }
  } else if(.fit_output[["model"]] == "ets"){
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(as.numeric(predict(.fit_output[["model_fit"]], attributes(x_data)[["prescription"]][["lag"]])[["mean"]]))
          , parameter = str_remove_all(.fit_output[["model_fit"]][["method"]], "ETS|NNAR|,|\\)|\\(")
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription$max_date + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(predict(.fit_output[["model_fit"]], horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  } else if(.fit_output[["model"]] == "neural_network"){
    if (tune == TRUE) {
      x_data %>%
        transmute(
          y_var_true = y_var,
          y_var_fcst = last(as.numeric(predict(.fit_output[["model_fit"]], attributes(x_data)[["prescription"]][["lag"]])[["mean"]]))
          , parameter = str_remove_all(.fit_output[["model_fit"]][["method"]], "ETS|NNAR|,|\\)|\\(")
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
          y_var_fcst = last(as.numeric(predict(.fit_output[["model_fit"]], attributes(x_data)[["prescription"]][["lag"]])[["mean"]]))
          , parameter = list(.fit_output[["model_fit"]][["parameters"]][["vect"]])
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
          y_var_fcst = last(as.numeric(snaive(.fit_output$y_var_int, attributes(x_data)[["prescription"]][["lag"]])[["mean"]]))
          , parameter = list(.fit_output[["model_fit"]][["parameters"]][["vect"]])
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription[["max_date"]] + months(1)), length.out = horizon, by = "months"),
        y_var_fcst = as.numeric(snaive(.fit_output$y_var_int, horizon)[["mean"]]),
        model = .fit_output[["model"]]
      )
    }
  }
}
  
  
