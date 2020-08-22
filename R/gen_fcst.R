get_forecast_experimental <- function(.fit_output, x_data = NULL, horizon = NULL, tune = FALSE) {
  prescription <- attributes(.fit_output)[["prescription"]]

  if (prescription$freq == 12) {
    freq_string <- "month"
  }

  if (.fit_output[["model"]] == "arima") {
    if (tune == TRUE) {
      x_data %>%
        transmute(
          .y_var_true = y_var,
          .y_var_pred = last(as.numeric(forecast(
            .fit_output[["model_fit"]],
            attributes(x_data)[["prescription"]][["lag"]]
          )[["mean"]])),
          .fit_output$parameter %>% t() %>% as_tibble()
        )
    } else {
      tibble(
        date = seq.Date(from = (prescription$max_date + months(1)), length.out = horizon, by = "months"),
        forecast = as.numeric(forecast(.fit_output[["model_fit"]], horizon)[["mean"]]),
        model = .fit_output$model
      )
    }
  } else if (.fit_output[["model"]] == "glmnet") {
    x_data_int <- make_reg_matrix(.fit_output = .fit_output, x_data = x_data, horizon = horizon)

    if (is.null(x_data) == TRUE) {

      # Synthetic x_data

      predict.glmnet(object = .fit_output$model_fit, newx = x_data_int) %>%
        as.vector() %>%
        enframe(name = "date", value = "forecast") %>%
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
          .y_var_true = y_var,
          .y_var_pred = as.numeric(predict.glmnet(object = .fit_output$model_fit, newx = x_data_int)),
          trend_discount = .fit_output$parameter$trend_discount,
          time_weight = .fit_output$parameter$time_weight,
          alpha = .fit_output$parameter$alpha,
          lambda = .fit_output$parameter$lambda
        )

      return(x_data_tmp)
    }
  } else if (.fit_output[["model"]][["glm"]]) {
    x_data_int <- make_reg_matrix(.fit_output = .fit_output, x_data = x_data, horizon = horizon)
    
    if (is.null(x_data) == TRUE) {
      predict.glm(object = .fit_output$model_fit, newx = x_data_int) %>%
        as.vector() %>%
        enframe(name = "date", value = "forecast") %>%
        mutate(
          date = seq.Date(
            from = (prescription$max_date + months(1)),
            by = freq_string,
            length.out = horizon
          ),
          model = "glm"
        )
    }
    

  }
}
