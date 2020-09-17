# Grid search prophet parameters

# Data

data_init <- read_csv("../hexyon_all.csv")
data_init <- data_init[,-1]
data_init <- data_init %>% 
  dplyr::filter(key == "hexyon_vol") 

cv_set <- data_init %>% 
  select(date_var,y_var) %>% 
  rename(ds = "date_var", y = "y_var")

date_seq <- tail(data_init$date_var,6)

# Grid

grid_number <- 30

rand_search_grid =  data.frame( 
  changepoint_prior_scale = sort(runif(grid_number, 0.1, 60)),
  seasonality_prior_scale = sort(sample(runif(grid_number, 0.1, 20))),
  n_changepoints          = rep(1:3,grid_number/3),
  Value                   = rep(0, grid_number)
)

# Loop

for (i in 1:nrow(rand_search_grid)) {
  parameters = rand_search_grid[i, ]
  error = c()
  for (d in date_seq) {
    train = subset(cv_set, ds < d)
    test  = subset(cv_set, ds == d)
    # Model
    m = prophet(train, growth = 'linear',
                seasonality.prior.scale = parameters$seasonality_prior_scale, 
                changepoint.prior.scale = parameters$changepoint_prior_scale,
                n.changepoints = parameters$n_changepoints,
                yearly.seasonality = F,
                weekly.seasonality = F,
                daily.seasonality = F)
    future = make_future_dataframe(m, periods = 1, freq = 'month')
    forecast = predict(m, future)
    forecast$ds = as.Date(forecast$ds)
    error_d = forecast::accuracy(forecast[forecast$ds %in% test$ds, 'yhat'], test$y)[ , 'MAPE']
    error = c(error, error_d)
  }
  mean_error = mean(error)
  print(mean_error)
  rand_search_grid$Value[i] = mean_error
}

# Output

best_cv_value = -1*rand_search_grid[which.max(rand_search_grid$Value),'Value']
rand_search_grid = arrange(rand_search_grid, desc(-Value))
head(rand_search_grid)

#---
