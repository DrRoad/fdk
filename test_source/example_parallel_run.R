# Main package

pkg <- c("autoforecast","glmnet", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly",
         "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW",
         "prophet", "forecTheta")

lapply(pkg, require, character.only = TRUE)

# Data import

data_init <- read_csv("test_source/exerci_tmp_check.csv") %>% 
  dplyr::select(-gbu) %>% 
  dplyr::mutate(
    reg_name = 0,
    reg_value = 0
  ) %>% 
  dplyr::arrange(key, date_var)

data_all <- data_init %>%
  prescribe_ts(key = "key", date_var = "date_var", y_var = "y_sales",
               reg_name = "reg_name", reg_value = "reg_value",
               freq = 12)

# Single test ----------------------------------------------------------

# data_test <- .data <- data_all %>% filter(key == unique(data_all$key)[22])
# 
# # run
# 
# model_list <- c("glm", "arima", "prophet", "dyn_theta", "ets", "croston")
# 
# 
# aux <- autoforecast(.data = data_test
#   , model = model_list
#   , parameter = NULL, optim_profile = "complete", test_size = 6
#   , lag = 3, meta_data = FALSE, method = "kalman", tune_parallel = TRUE
#   , number_best_models = 1, pred_interval = TRUE
# )
# 
# aux %>% plot_ts()

# Multiple items / Parallel ----------------------------------------------------------

model_list <- c("glm", "glmnet", "croston", "arima", "ets")

cluster = makeCluster(4, type = "SOCK")
registerDoSNOW(cluster)
ntasks <- length(unique(data_all$key))
progress <- function(n) {
  cat(sprintf(" %d Keys(s) / %.2f%% percent remaining\n",ntasks-n,(ntasks-n)*100/ntasks))
}
opts <- list(progress=progress)

tictoc::tic()
results <- foreach(key_i = unique(data_all$key), .errorhandling='stop', .combine = "rbind",
                   .options.snow=opts, .packages = pkg) %dopar% {
  data_i <- data_all[data_all$key == key_i,]
  autoforecast(.data = data_i, horizon = 24
               , model = model_list
               , parameter = NULL, optim_profile = "light", test_size = 6
               , lag = 3, meta_data = FALSE, method = "kalman", tune_parallel = TRUE
               , number_best_models = 1, pred_interval = TRUE
               , metric = "mape")
}
tictoc::toc()

stopCluster(cluster)

# Results

write.csv(results,"test_source/diagnostics.csv")

# Plot

plot_res <- results %>% filter(key == unique(data_all$key)[1] & !model == "ensemble") %>%
  plot_ts(interactive = F)

plot_res

#---
