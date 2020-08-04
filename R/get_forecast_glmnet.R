
# get forecast

get_forecast <- function(fit_output, test=NULL, horizon=NULL, inherited_details = FALSE){
  if(is.null(horizon)==T){
    if(grepl(x = paste0(class(fit_output$fit), collapse = ""), "glmnet")){
      if(is.null(fit_output[["config"]][["lag"]])==TRUE & nrow(test)==1){
        stop("Accuracy performance mode is expected, nonetheless no lag (horizon) has been set in config")
      }
      trend_discount_tmp <- get_trend_discounts(data_length = fit_output[["model_summary"]][["train_size"]]
                                                , trend_discount = fit_output$param$trend_discount
                                                , lag = NULL) # to provide whole vector
      
      test[["trend"]] <- trend_discount_tmp[fit_output[["config"]][["lag"]]]
      x_test <- test[,-1] %>% # first column as dependent
        fastDummies::dummy_cols(select_columns = fit_output[["model_summary"]][["factor_vars"]]
                                , remove_first_dummy = TRUE
                                , remove_selected_columns = TRUE) %>%
        select(-date) %>% 
        as.matrix()
      if(inherited_details == FALSE){
        list(y_true = test[,1][[1]]
             , y_pred = as.vector(predict(fit_output[["fit"]], newx = x_test))
             , error = (test[,1][[1]] - as.vector(predict(fit_output[["fit"]], newx = x_test)))
        )
      } else {
        list(y_true = test[,1][[1]]
             , y_pred = as.vector(predict(fit_output[["fit"]], newx = x_test))
             , error = (test[,1][[1]] - as.vector(predict(fit_output[["fit"]], newx = x_test)))
             , fit_output = fit_output)
      }
    }
  } else {
    test <- make_reg_matrix(fit_output, horizon)
    x_forecast <- test %>% # first column as dependent
      fastDummies::dummy_cols(select_columns = fit_output[["model_summary"]][["factor_vars"]]
                              , remove_first_dummy = FALSE
                              , remove_selected_columns = FALSE) %>%
      select(all_of(fit_output$model_summary$x_names)) %>% 
      as.matrix()
    if(inherited_details == FALSE){
      tibble(date = test[["date"]]
             , y_pred = as.vector(predict(fit_output[["fit"]], newx = x_forecast)))
    } else {
      list(forecast = tibble(date = test[["date"]]
                             , y_pred = as.vector(predict(fit_output[["fit"]], newx = x_forecast)))
           , fit_output = fit_output)
    }
  }
}
