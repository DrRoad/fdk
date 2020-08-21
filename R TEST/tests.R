# Cleansing ---------------------------------------------------------------

# Fit ---------------------------------------------------------------------

grid <- expand_grid(time_weight = seq(from = 0.7, to = 1, by = 0.05)
                    , trend_discount = seq(from = 0.8, to = 1, by = 0.05)
                    , alpha = seq(from = 0, to = 1, by = 0.10))

parameter <- list(glmnet = list(time_weight = 0.9, trend_discount = .9, alpha = 0, lambda = 0
                                , grid = grid
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.05
                                             , n_best_model = 1)))

get_predictions <- function(splits, random_grid){
  get_glmnet(.data = splits[["train"]], parameter = update_parameter(parameter, grid[random_grid,])) %>% 
    get_forecast_experimental(x_data = splits[["test"]], tune = TRUE)
}

tictoc::tic()
tune_ts(.data, test_size = 6, lag = 4, parameter = parameter)
tictoc::toc()







