
# get_ensemble

eat_ensemble <- function(y) {
  require(forecast)
  fets <- forecast(ets(y), h)
  farima <- forecast(auto.arima(y), h)
  ftheta <- thetaf(y, h)
  comb <- fets
  comb$mean <-(fets$mean + farima$mean + ftheta$mean)/3
  comb$method <- "ETS-ARIMA-Theta Combination"
  return(comb)
}

#---