# Intro

AutoForecast provides a collection of automated procedures to analyze, forecast time series data, and, generate insights for decision makers. The package has been designed to be modular and fast, giving the possibility to the user to get the results according to their needs frustration-free. The modules pack individual function that achieve the main steps of a typical Machine Learning project but centered on time-series data:

1. Pre-processing
2. Data wrangling
3. Data exploration
4. Feature engineering
5. Cleansing and imputation
6. Model fitting
7. Time Series Hyperaparameter tuning
8. Forecast

Autoforecast is also accompanied by a front-end and cloud solution to improve user experience.

# Installation

The installation can be performed directly from GitLab. 
You can install it using the following code and using your personal GitLab account. 

```r
library(devtools)
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)
pkg <- "https://emea-aws-gitlab.sanofi.com:3001/sc_analytics_coe/statistical-forecasting/packages/autoforecast.git"
cred <- git2r::cred_user_pass(rstudioapi::askForPassword("Username"), rstudioapi::askForPassword("Password"))
devtools::install_git(pkg, credentials = cred)
```
