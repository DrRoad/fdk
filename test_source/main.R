# Main package

pkg <- c("glmnet", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly",
         "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW",
         "prophet", "forecTheta", "autoforecast")

lapply(pkg, require, character.only = TRUE)

# Parameters -------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.92, to = 0.96, by = 0.02)
                           , trend_discount = seq(from = 0.68, to = 0.72, by = 0.02)
                           , alpha = seq(from = 0, to = 1, by = 0.1))
grid_glm <- expand_grid(time_weight = seq(from = 0.96, to = 1, by = 0.02)
                        , trend_discount = seq(from = 0.65, to = 0.75, by = 0.02))

# Parameter list -------------------------------------------------------------

parameter <- list(glmnet = list(time_weight = .94, trend_discount = .70, alpha = 0, lambda = .1
                                , grid_glmnet = grid_glmnet
                                , job = list(optim_lambda = TRUE, x_excluded = NULL
                                             , random_search_size = 0.05
                                             , n_best_model = 1))
                  , croston = list(alpha = 0.1)
                  , glm = list(time_weight = .99, trend_discount = 0.70
                               , grid_glm = grid_glm
                               , job = list(x_excluded = NULL
                                            , random_search_size = 0.1
                                            , n_best_model = 1))
                  , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                  , ets = list(ets = "ZZZ"))

# Data import

data_init <- read_csv("test_source/demo_data.csv") %>% 
  dplyr::filter(date < "2020-02-01")

data_all <- data_init %>%
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value")

# Multiple items / Parallel ----------------------------------------------------------

model_list <- c("glm", "glmnet", "dynamic_theta", "prophet", "arima", "ets","seasonal_naive", "croston")

cluster = makeCluster(4, type = "SOCK")
registerDoSNOW(cluster)
ntasks <- length(unique(data_all$key)[1:5])
progress <- function(n) {
  cat(sprintf(" %d Keys(s) / %.2f%% percent remaining\n",ntasks-n,(ntasks-n)*100/ntasks))
}
opts <- list(progress=progress)

tictoc::tic()
results <- foreach(key_i = unique(data_all$key), .combine = "rbind"
                   , .options.snow=opts, .packages = pkg) %dopar% {
  data_i <- data_all[data_all$key == key_i,]
  autoforecast(.data = data_i, horizon = 24
               , model = model_list
               , parameter = parameter, optim_profile = "light", test_size = 6
               , lag = 3, meta_data = FALSE, method = "winsorize", tune_parallel = TRUE
               , number_best_models = 3, pred_interval = TRUE)
}
tictoc::toc()

stopCluster(cluster)

# Plot

# plot_res <- results %>% filter(key == unique(data_all$key)[3]) %>% 
#   plot_ts(interactive = F)
# 
# plot_res

#---
