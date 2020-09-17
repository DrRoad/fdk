# Package -----------------------------------------------------------------

pkg <- c("glmnet", "forecast", "stlplus", "fastDummies", "imputeTS", "plotly"
         , "tidyverse", "doParallel", "foreach", "parallel", "tsibble", "doSNOW"
         , "forecTheta", "prophet")

invisible(lapply(pkg, require, character.only = TRUE))

# Source ------------------------------------------------------------------

source("R/get_seasonal_naive.R")
source("R/get_forecast.R")
source("R/get_ets.R")
source("R/get_arima.R")
source("R/get_glm.R")
source("R/get_glmnet.R")
source("R/get_croston.R")
source("R/get_neural_network.R")
source("R/get_tbats.R")
source("R/cleansing.R")
source("R/auxiliar.R")
source("R/autoforecast.R")
source("R/feature_engineering.R")
source("R/optim_ts.R")
source("R/get_dyn_theta.R")
source("R/get_tslm.R")
source("R/get_prophet.R")

# Parameter ---------------------------------------------------------------

grid_glmnet <- expand_grid(time_weight = seq(from = 0.9, to = 1, by = 0.02)
                           , trend_discount = seq(from = 0.95, to = 1, by = 0.01)
                           , alpha = seq(from = 0, to = 1, by = 0.10))
grid_glm <- expand_grid(time_weight = seq(from = 0.9, to = 1, by = 0.02)
                        , trend_discount = seq(from = 0.9, to = 1, by = 0.02))

parameter <- list(glmnet = list(time_weight = 1.0, trend_discount = .91, alpha = .7, lambda = 320
                                , grid_glmnet = grid_glmnet
                                , job = list(optim_lambda = FALSE, x_excluded = NULL
                                             , random_search_size = 0.1
                                             , n_best_model = 1))
                  , croston = list(alpha = 0.1)
                  , glm = list(time_weight = 1.0, trend_discount = .91
                               , grid_glm = grid_glm
                               , job = list(x_excluded = NULL
                                            , random_search_size = 0.3
                                            , n_best_model = 1))
                  , arima = list(p = 1, d = 1, q = 0, P = 1, D = 0, Q = 0)
                  , ets = list(ets = "ZZZ"))

# Data import

data_init <- read_csv("test_source/demo_data.csv") %>% 
  dplyr::filter(date < "2020-02-01"
                , forecast_item != "FI: 34142")

data_all <- data_init %>%
  prescribe_ts(key = "forecast_item", y_var = "volume", date_var = "date"
               , freq = 12, reg_name = "reg_name", reg_value = "reg_value")


## Dupixent

us0 <- read_rds("test_source/us_dupixent.rds") %>% 
  mutate(reg_name = case_when(
    year_month == "2019-12-01" & key == "US: 710613" ~ "christmas"
    , TRUE ~ reg_name)
    , reg_value = ifelse(year_month == "2019-12-01" & key == "US: 710613", 1, reg_value))

# Single item forecast / modularity ---------------------------------------

### Default parameters

fit_1 <- data_all %>% 
  filter(key == "DK: 578281") %>%
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "winsorize") %>% # options: winsorize (default), nearest, mean, median. 
  fit_ts(model = "ets", parameter = parameter)

#### Fit output
  #summary(fit_1)
  attributes(data_all) %>% 
    str()
  
#### Forecast
fit_1 %>% 
  get_forecast(horizon = 36) # horizon creates a synthetic data, for counterfactual use argument "x_data".

### Hyperparameter tuning

data_all %>% 
  filter(key == "DK: 578281") %>% 
  feature_engineering_ts() %>% # automatically creates features of: trend and seasonal_var factor given inherited prescription.
  clean_ts(method = "kalman") %>% # options: winsorize (default), nearest, mean, median. 
  optim_ts(test_size = 6, lag = 3, parameter = parameter, model = "glmnet")


## Optimization ------------------------------------------------------------

model_list  <- c("glm", "glmnet", "neural_network", "arima", "ets", "seasonal_naive", "croston","tbats","dynamic_theta","tslm")
model_list  <- c("glm", "glmnet")


dupi <- read_excel("test_source/dupixent_us.xlsx") %>% 
  mutate(date = as.Date(date))

us_all <- dupi %>% 
  filter(date < "2020-01-01") %>% 
  prescribe_ts(key = "key", y_var = "volume", date_var = "date"
               , reg_name = "reg_name", reg_value = "reg_value", freq = 12)

.data <- us_all %>% 
  dplyr::filter(key == "US: 710613") %>% 
  feature_engineering_ts() %>% 
  clean_ts(method = "kalman")

  
  optim_ts(test_size = 3, lag = 1, parameter = parameter, model = "glmnet")
  fit_ts(model = "glmnet", parameter = parameter) %>% 
  get_forecast(horizon = 12)
  ggplot(aes(date_var, y_var))+
  geom_line()

fast_optim_forecast <- autoforecast(.data = .data
                                    , horizon = 6
                                    , model = model_list
                                    , parameter = parameter
                                    , optim_profile = "fast"
                                    , method = "kalman"
                                    , number_best_models = 3
                                    , pred_interval = FALSE)
fast_optim_forecast %>% 
  plot_ts(interactive = T)

fast_optim_forecast %>% 
  clipr::write_clip()

## Light




set.seed(1)
my_cores <- detectCores()
registerDoParallel(cores = (my_cores - 2))

light_optim_forecast <- autoforecast(.data = .data
                                     , horizon = 6
                                     , model = model_list
                                     , parameter = parameter
                                     , optim_profile = "light"
                                     , test_size = 6
                                     , lag = 1
                                     , meta_data = FALSE
                                     , tune_parallel = FALSE
                                     , pred_interval = FALSE
                                     , number_best_models = 3
                                     , method = "winsorize")

light_optim_forecast %>% 
  plot_ts(interactive = T)


cluster = makeCluster(4, type = "SOCK")
registerDoSNOW(cluster)
ntasks <- length(unique(data_all$key)[1:2])
progress <- function(n) {
  cat(sprintf(" %d Keys(s) / %.2f%% percent remaining\n",ntasks-n,(ntasks-n)*100/ntasks))
}
opts <- list(progress=progress)

results <- foreach(key_i = unique(us$key)
                   , .combine = "rbind"
                   , .options.snow=opts, .packages = pkg) %dopar% {
  data_i <- us[us$key == key_i,]
  autoforecast(.data = data_i, horizon = 100
               , model = model_list
               , parameter = parameter, optim_profile = "light", test_size = 6
               , lag = 3, meta_data = FALSE, method = "winsorize", tune_parallel = TRUE)
}

stopCluster(cluster)


# Plotting

results %>% 
  plot_ts(multiple_keys = T, interactive = T)




# Hexyon ------------------------------------------------------------------

h1 <- read_csv("test_source/hexyon_all.csv")

h1 %>% 
  do({
    tmp <- tibble(.) %>% 
      filter(key == "hexyon_vol") %>% 
      select(-reg_name, -reg_value)
    ms <- tibble(.) %>% 
      filter(key == "hexyon_ms") %>% 
      select(date_var, reg_name = key, reg_value = y_var)
    left_join(
      tmp, ms, by = "date_var"
    )
  }) %>% 
  saveRDS(file = "hexyon_regressor.rds")
