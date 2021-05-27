# Functions ---------------------------------------------------------------

#' Calculate Marginal Seasonal Effects
#'
#' @param .data tibble/data.frame
#' @param min_date date: set mininum date.
#' @param max_date date: set maximum date.
#' @param robust_lm logical: whether or not to use robust regression.
#' @param ci_me numeric: confidence interval for marginal effects.
#' 
#' @importFrom MASS rlm
#' @import tidyverse
#' @import ggeffects
#' @import janitor
#'
#' @return
#' @export
#'
#' @examples
get_seas_me <- function(.data, min_date = NULL
                        , max_date = NULL, robust_lm = F
                        , ci_me = .95){
  
  z_ce <- tibble::tribble(
    ~ci,   ~z,
    0.80, 1.282,
    0.85,  1.44,
    0.90, 1.645,
    0.95,  1.96,
    0.99, 2.576,) %>% 
    filter(ci == ci_me) %>% 
    pull(z)
  
  if(robust_lm == T){
    lm_reg <- match.fun("rlm")
  } else {
    lm_reg <- match.fun("glm")
  }
  
  if(is.null(min_date)==T){
    min_date <- min(.data[["date_var"]])
  }
  
  if(is.null(max_date)==T){
    max_date <- max(.data[["date_var"]])
  }
  
  .data %>% 
    dplyr::filter(date_var >= min_date
                  , date_var <= max_date) %>% 
    dplyr::select(-matches("date_var|y_var|trend"), y_var_d1) %>% 
    lm_reg(y_var_d1~., data = .) %>% 
    ggeffects::ggeffect(terms = c("month_seas")) %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(predicted = round(predicted, 2)
           , conf_low = predicted - z_ce * std_error
           , conf_high = predicted + z_ce * std_error
           , month_seas = factor(x, levels = month.abb)) %>% 
    dplyr::select(month_seas, everything(), -x, -group)
}


#' Get GAM features
#'
#' @param oc_data tibble
#' @param key string: name of the key to be processed.
#' @param parameter list: parameters and hyperparameters.
#' 
#' @import shinyWidgets
#' @import plotly
#' @import shinydashboard
#' @import shinycssloaders
#' @import shinyjs
#' @import tidyverse
#' @import shinyWidgets
#' @import formattable
#' @import tidyverse
#' @import janitor
#' @import gratia
#' @import mgcv
#'
#' @return
#' @export
#'
#' @examples
get_insight_data <- function(oc_data, key, parameter){
  key_in <- key
  
  data_init <- oc_data$sales %>% 
    filter(forecast_item == key_in)
  data_init <- data_init[!rev(cumsum(rev(data_init$sales))==0), ]
  date_vec <- data_init %>% 
    filter(reg_name == "history") %>% 
    pull(date) %>% 
    unique()
  data_init <- data_init %>% 
    group_split(date <= max(date_vec), .keep = F)
  
  hd <- data_init %>%
    map(~{
      pd <- prescribe_ts(.x, key = "forecast_item", y_var = "sales"
                 , date_var = "date", reg_name = "reg_name"
                 , reg_value = "reg_value", freq = 12, date_format = "ymd") %>% 
      pull(data) %>% 
      .[[1]] %>% 
      validate_ts() %>% 
      feature_engineering_ts() %>% 
      dplyr::select(-any_of("history"))
      
      fit_in <- fit_ts(.data = pd, ts_model = "gam", parameter = parameter)
      
      sum_0 <- summary(fit_in)
      sum_1 <- sum_0$p.table %>%
        as.data.frame() %>% 
        tibble::rownames_to_column("term") %>% 
        as_tibble(.name_repair = janitor::make_clean_names) %>% 
        mutate(across(where(is.numeric), ~round(.x, 2))
               , term_type = "fixed") %>% 
        dplyr::select(term, term_type, estimate, p_value = 5)
      
      sum_2 <- tryCatch(
        {
          sum_0$s.table %>% 
            as.data.frame() %>% 
            tibble::rownames_to_column("term") %>% 
            as_tibble(.name_repair = janitor::make_clean_names) %>% 
            mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
            mutate(term_type = "smoothed", .after = 1) %>% 
            mutate(term = str_remove_all(term, "^s\\(|\\)$")) %>% 
            dplyr::select(1, 2, estimate = edf, p_value)
        }
        , error = function(x) tibble(term = character()
                                     , term_type = character()
                                     , estimate = numeric()
                                     , p_value = numeric())
        )
      
      sum_join <- bind_rows(sum_1, sum_2) %>% 
        mutate(is_sig = case_when(
          p_value < .01 ~ "***"
          , p_value < .05 ~ "**"
          , p_value < .1 ~ "*"
          , TRUE ~ ""))
      
      fitted_in <- predict(fit_in, newdata = pd, se.fit = T, type = "response") %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        rowwise() %>% 
        mutate(lwr = fit - 1.96*se.fit
               , upr = fit + 1.96*se.fit) %>% 
        ungroup() %>% 
        janitor::clean_names()
      
      pd_ext <- pd %>% 
        bind_cols(fitted_in)
      
      derivative_in <- tryCatch(
        {
          fit_in %>% 
            gratia::derivatives(n = 100) %>% 
            as_tibble() %>%
            rename(trend = data, lwr = lower, upr = upper
                   , fit_se = se, trend_deriv = derivative)
        }
        , error = function(err) {tibble(error = NA_real_)}
      )
      
      seas_me <- pd_ext %>% 
        mutate(y_var_d1 = tsibble::difference(y_var)) %>% 
        na.omit() %>% 
        get_seas_me()
    
      list(gam_fitted = pd_ext
           , pd_data = pd
           , fit = fit_in
           , gam_trend_deriv = derivative_in
           , seas_me = seas_me
           , fit_summary = sum_join)
  })
  
  # GAM Forecast
  
  new_data <- hd[[1]]$pd_data %>% 
    mutate(trend = get_trend_decay(y_var_length = length(hd[[2]]$pd_data$y_var)
                                   , trend_decay = parameter$gam$trend_decay
                                   , horizon = length(trend))) %>%
    dplyr::select(-any_of(c("date_var", "y_var")))
   # mutate(trend = length(unique(date_vec)) +  trend)

  forecast_gam <- predict.gam(object = hd[[2]]$fit, newdata = new_data
                              , se.fit = T, type = "response") %>% 
    dplyr::bind_cols() %>% 
    janitor::clean_names() %>% 
    mutate(lwr = fit - 1.96 * se_fit
           , upr = fit + 1.96 * se_fit
           , date = hd[[1]]$pd_data$date_var) %>% 
    set_names(nm = paste0(names(.), "_gam")) %>% 
    rename(forecast_gam = fit_gam) %>% 
    tidyr::unnest(cols = c(1:last_col()))
  
  # Zero when negative sales
  
  if(any(forecast_gam$forecast_gam<0)){
    forecast_gam <- forecast_gam %>% 
      mutate(across(.cols = 1:4
                    , .fns = ~if_else(.x<0, 0, .x)))
  }
  
  forecast_gam_deriv <- tryCatch(
    {
      forecast_gam %>% 
        dplyr::select(forecast_gam, date_gam) %>%
        mutate(key = "fcst", reg_value = NA_real_, reg_name = "") %>% 
        prescribe_ts(y_var = "forecast_gam"
                     , reg_name = "reg_name"
                     , reg_value = "reg_value"
                     , key = "key"
                     , date_var = "date_gam"
                     , freq = 12
                     , date_format = "ymd") %>% 
        pull(data) %>% 
        .[[1]] %>% 
        validate_ts() %>% 
        feature_engineering_ts() %>% 
        mutate(trend = new_data$trend) %>% 
        fit_ts(ts_model = "gam", parameter = parameter) %>% 
        gratia::derivatives(n = 100) %>% 
        as_tibble() %>%
        rename(trend = data, lwr = lower, upr = upper
               , fit_se = se, trend_deriv = derivative) %>% 
        mutate(history = "forecast_gam")
    }
    , error = function(err) tibble(smooth = character()
                                , var = character()
                                , trend = numeric()
                                , trend_deriv = numeric()
                                , fit_se = numeric()
                                , crit = numeric()
                                , lwr = numeric()
                                , upr = numeric()
                                , history = character()
    )
  )

  # Features
  
  hd[[1]]$gam_trend_deriv <- tryCatch(
    {
      hd[[1]]$gam_trend_deriv %>% 
        mutate(trend = trend + length(unique(date_vec)))
    }
    , error = function(err) NULL
    )
  
  gam_fitted <- hd[[2]]$gam_fitted %>%
    mutate(history = T) %>% 
    bind_rows(hd[[1]]$gam_fitted) %>% 
    replace_na(replace = list(history=F)) %>% 
    left_join(forecast_gam, by = c("date_var" = "date_gam"))
    
  gam_deriv_trend <- hd[[2]]$gam_trend_deriv %>% 
    mutate(history = "history") %>% 
    bind_rows(hd[[1]]$gam_trend_deriv) %>% 
    replace_na(replace = list(history="forecast")) %>% 
    bind_rows(forecast_gam_deriv)
  
  seas_me <- hd[[2]]$seas_me %>% 
    mutate(history = T) %>% 
    bind_rows(hd[[1]]$seas_me) %>% 
    replace_na(replace = list(history=F))
  
  # Cummulative difference between forecasts
  
  stats <- gam_fitted %>% 
    filter(history == F) %>% 
    dplyr::select(date_var, y_var, fit, forecast_gam) %>% 
    mutate(y_var_cum = cumsum(y_var)
           , fit_gam_cum = cumsum(forecast_gam)) %>% 
    rowwise() %>% 
    mutate(fit_diff = y_var - forecast_gam
           , fit_diff_perc = round(y_var/forecast_gam - 1, 4)
           , fit_cum_diff_perc = round(y_var_cum/fit_gam_cum - 1, 4)) %>% 
    ungroup() %>% 
    dplyr::select(-y_var, -fit, - forecast_gam)
  
  gam_fitted <- gam_fitted %>% 
    left_join(stats, by = "date_var")
  
  n_months_ahead_forecast <- length(stats$date_var)
  cuts_month <- c(1, 6, 12, 24)
  cuts_month <- cuts_month[cuts_month <= n_months_ahead_forecast]

  cum_diff <- stats %>% 
    slice(cuts_month) %>% 
    mutate(months_ahead = cuts_month) %>% 
    dplyr::select(months_ahead, fit_cum_diff_perc)
  
  # Year aggregation
  
  current_year <- hd[[1]]$pd_data %>% 
    pull(date_var) %>% 
    min() %>% 
    lubridate::year()
  
  history_year <- gam_fitted %>% 
    filter(history == T) %>% 
    pull(date_var) %>% 
    max() %>% 
    lubridate::year()
  
  completed_years <- gam_fitted %>% 
    dplyr::select(date_var) %>% 
    group_by(year = lubridate::year(date_var)) %>% 
    count() %>% 
    filter(n == 12) %>% 
    pull(year)
  
  year_agg <- gam_fitted %>% 
    dplyr::select(date_var, y_var, forecast_gam) %>% 
    mutate(shared_history = case_when(
      lubridate::year(date_var) == history_year ~ T
      , TRUE ~ F)) %>% 
    mutate(forecast_gam = case_when(
      shared_history == T & is.na(forecast_gam) ~ y_var
      , TRUE ~ forecast_gam
    )) %>% 
    pivot_longer(cols = c("y_var", "forecast_gam")) %>%
    group_by(year = lubridate::year(date_var), name) %>% 
    summarise(sales = sum(value, na.rm = T)
              , .groups = "drop") %>% 
    filter(year %in% completed_years) %>% 
    mutate(sales = case_when(
      name == "forecast_gam" & year < current_year ~ NA_real_
      , TRUE ~ sales
    )) %>% 
    pivot_wider(names_from = name, values_from = sales) %>% 
    mutate(ref_vs_gam = y_var - forecast_gam
           , ref_vs_gam_perc = round(ref_vs_gam/y_var, 4)
           , ref_diff = round(y_var/lag(y_var)-1, 4)
           , gam_diff = round(forecast_gam/lag(forecast_gam)-1, 4)) %>%
    rename(ref = y_var, gam = forecast_gam) %>% 
    mutate(across(.cols = c("ref", "gam")
                  , .fns = list(month_mean = ~ round(.x/12,0)))) %>% 
    dplyr::select(year, ref, gam, everything())
  
  

  # Feature -----------------------------------------------------------------

  feature_imp <- hd[[2]]$fit_summary %>% 
    filter(!str_detect(term, "_seas|Interce"))
  
  # Return ------------------------------------------------------------------

  list(key = key_in
       , gam_fitted = gam_fitted
       , gam_deriv_trend = gam_deriv_trend
       , seas_me = seas_me
       , summary_stats = list(cum_diff = cum_diff
                              , year_agg = year_agg
                              , feature_imp = feature_imp))
}

#' Generate graphics for time series insights
#'
#' @param insight_data tibble.
#' @param graph_type string: {derivative, forecast, marginal}.
#' @param conf list: graph options.
#' 
#' @import tidyverse
#' @import shiny
#' @import stringr
#' @importFrom plotly ggplotly
#' @importFrom plotly layout 
#' @import mgcv
#' @import tidyr
#' 
#' @return
#' @export
#'
#' @examples
get_graph_stat <- function(insight_data, graph_type, conf = list(min_limit_zero = F
                                                                 , gam_forecast = F)){
  
  if(graph_type == "forecast"){
    if(conf$min_limit_zero == T){
      tmp <- insight_data$gam_fitted %>% 
        mutate(across(.cols = c("fit", "lwr", "upr", "upr_gam", "lwr_gam", "forecast_gam"), ~ifelse(.x<0, 0, .x)))
        scale_y_continuous(limits = c(0, max(insight_data$gam_fitted$upr))
                           , n.breaks = 10, minor_breaks = NULL)
    } else {
      tmp <- insight_data$gam_fitted
    }
    
    has_reg <- !all(str_detect(names(tmp), "date_var|y_var|^trend|_seas|fit|lwr|upr|history|cum|forecast_gam"))
    
    if(has_reg == T){
      y_var_na <- tryCatch({
        tmp %>% 
          dplyr::select(-matches("date_var|fit|_seas|lwr|upr|history|trend|cum|forecast_gam")) %>% 
          mutate(across(2:last_col(), .fns = ~.x!=0)) %>%
          mutate(ind = rowSums(.[c(2:ncol(.))])) %>% 
          mutate(ind = ifelse(ind!=0, y_var, NA_real_)) %>% 
          pull(ind)
      }, error = function(err) pull(tibble(y_var_na = NA, .rows = nrow(tmp)))
      )
      
      label <- tmp %>% 
        dplyr::select(-matches("y_var|fit|_seas|lwr|upr|history|trend|cum|forecast_gam")) %>% 
        pivot_longer(cols = 2:last_col()) %>% 
        filter(value !=0) %>% 
        group_by(date_var) %>% 
        summarise(label = paste0(name, collapse = ", "))
      
      tmp <- tmp %>%
        mutate(y_var_na = y_var_na) %>% 
        left_join(label, by = "date_var")
    }
    
    
    # limits
    
    g_lims <- tmp %>% 
      #dplyr::select(matches("lwr|upr|y_var")) %>% 
      dplyr::select(any_of(c("lwr", "upr", "y_var"))) %>%
      unlist() %>% 
      na.omit() %>% 
      range()
    
    g_lims[1] <- g_lims[1]*.8
    g_lims[2] <- g_lims[2]*1.2
    
    g_tmp <- tmp %>% 
      ggplot()+
      geom_ribbon(aes(date_var, ymin = lwr, ymax= upr, fill = history), alpha = .2)+
      geom_line(aes(date_var, y_var, col = history))+
      geom_line(aes(date_var, fit, col = history), linetype ="dashed", size = 1)+
      scale_x_date(date_breaks = "4 month", date_labels = "%b-%y", minor_breaks = NULL)+
      labs(y = "Sales", x = "", title = insight_data$key)+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90)
            , legend.position="bottom")+
      geom_hline(yintercept = 0, linetype = "dotted") +
      scale_y_continuous(n.breaks = 10, minor_breaks = NULL)+
      theme(legend.position =  "none")
    
    if(conf$gam_forecast == T){
      g_tmp <- g_tmp + 
        # geom_ribbon(aes(date_var, ymin = lwr_gam
        #                 , ymax = upr_gam), fill = "purple", alpha = .2)+
        geom_line(aes(date_var, forecast_gam), col = "green")
    }
    
    if(has_reg == T){
      g_tmp <- g_tmp +
        geom_point(aes(date_var, y_var_na, shape = label)
                   , show.legend = T, na.rm = TRUE)+
        scale_shape_discrete(na.translate = F)
    }
    
    ggplotly(g_tmp, dynamicTicks = F, layerData = T) %>% 
      layout(legend = list(orientation = "h", x = .7, y = 0.96)
             , yaxis = list(autorange = T))
    
  } else if(graph_type == "derivative"){
    g_tmp <- insight_data$gam_deriv_trend %>% 
      ggplot()+
      geom_hline(yintercept = 0, linetype = "dashed")+
      geom_ribbon(aes(trend, ymin = lwr, ymax = upr
                      , fill = history), alpha = .2)+
      geom_line(aes(trend, trend_deriv, col = history))+
      labs(x = "Time index (continuous)", y = "Trend derivative")+
      theme_minimal()+
      theme(legend.position = "none")
    
    ggplotly(g_tmp)
    
  } else if(graph_type == "seas_me"){
    g_tmp <- insight_data$seas_me %>% 
      ggplot()+
      geom_hline(yintercept = 0, linetype = "dotted", col = "red", size = 1)+
      geom_linerange(aes(x = month_seas, ymin = conf_low, ymax = conf_high
                         , col = history)
                     , position = position_dodge(width = .3)
                     #, col="black"
                     )+
      geom_point(aes(month_seas, predicted
                     , col = history), position = position_dodge(width = .3)
                 , size = 2)+
      theme_minimal()+
      theme(legend.position="none")+
      # theme(axis.text = element_text(colour = "black", size = 12)
      #       , axis.title = element_text(colour = "black", size = 15))+
      labs(x = "", y = "Marginal Seasonal Effects")
    
    ggplotly(g_tmp) %>% 
      layout(legend = list(orientation = "h", x = .45, y = .96))
  }
  
}

#' Internal GAM parameter
#'
#' @param parameter list: parameters and hyperparameters.
#' @param values_list list: parameter modifiers.
#'
#' @return
#'
#' @examples
react_gam_par <- function(parameter, values_list){
  if(values_list$k != -1){
    parameter$gam$smoothed_features$trend$k <- values_list$k
  }
  
  if(values_list$link_function == "gaussian"){
    parameter$gam$smoothed_features$trend$k <- "gaussian" # poisson does not work well
    #parameter$gam$smoothed_features$trend$k <- values_list$link_function
  }
  
  if(values_list$trend_decay !=1){
    parameter$gam$trend_decay <- values_list$trend_decay
  }
  
  if(any(is.null(values_list$exc_features)) == F){
    parameter$gam$excluded_features <- c(values_list$exc_features)
  }
  return(parameter)
}


#' Internal modification of statistical data
#'
#' @param insight_data tibble/data.frame
#' @param date date: set minimum date.
#'
#' @return
#'
#' @examples
mod_stat_data <- function(insight_data, date = NULL){
  # if(date == ""){
  #   date <- "1899-01-01"
  # }
  
  tryCatch({
    data_hist <- insight_data$gam_fitted %>% 
      filter(date_var >= date)
    
    insight_data$gam_fitted <- data_hist
    return(insight_data)
  }
  , error = function(err) insight_data)
}


#' Generate dashboard tables
#'
#' @param insight_data tibble
#' @param table_type character
#'
#' @importFrom purrr map_chr
#' @import formattable
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_tables()
#' }
get_tables <- function(insight_data, table_type){
  
  get_thousand_int <- function(x){
    x_abs <- abs(na.omit(x))
    
    if(any(x_abs > 999999)){
      scale_int <- 1/1e6
      suffix_int <- "M"
      acc_int <- .01
    } else if(any(x_abs > 999)){
      scale_int <- 1/1e3
      suffix_int <- "K"
      acc_int <- .01
    } else {
      scale_int <- 1
      suffix_int <- ""
      acc_int <- 1
    }
    
    func_int <- purrr::possibly(
      scales::number_format(scale = scale_int
                                , accuracy = acc_int
                                , suffix = suffix_int)
      , otherwise = NA)
    
    map_chr(x, ~func_int(.x))
  }
  
  get_perc_int <- function(x, type){
    if(type == "arrow"){
      func_int <- purrr::possibly(function(x){
        if(x > 0){
          "arrow-up"
        } else if(x < 0){
          "arrow-down"
        } else {
          ""
        }
      }, otherwise = NA)
    } else {
      func_int <- purrr::possibly(function(x){
        if(x > 0){
          "red"
        } else if(x < 0){
          "green"
        } else {
          ""
        }
      }, otherwise = NA)
    }
    map_chr(x, ~func_int(round(.x, 3)))
  }
  
  thousand_format <- formatter("span", x ~ get_thousand_int(x))

  perc_format <- formatter("span"
                        , x ~ percent(x, digits = 1)
                        , x ~ icontext(get_perc_int(x, type = "arrow"))
                        #, style = formattable::style("color" = ~get_perc_int(x, type = "color"))
                        )
  
  # Cummulative difference --------------------------------------------------

  if(table_type == "cum_diff"){

    return(
    formattable(insight_data$summary_stats$cum_diff
                , align = c("c", "c")
                , list(
                  `fit_cum_diff_perc` = perc_format
                ))
    )
  }
  
  if(table_type == "year_agg"){
    
    
    col_names <- colnames(insight_data$summary_stats$year_agg)[-1]
    f2 <- list(
      gam = thousand_format
      , ref = thousand_format
      , ref_vs_gam = thousand_format
      , ref_vs_gam_perc = perc_format
      , ref_diff = perc_format
      , gam_diff = perc_format
      , gam_month_mean = thousand_format
      , ref_month_mean = thousand_format
    )
  
    return(
    insight_data$summary_stats$year_agg %>%
      mutate(across(.cols = c("ref", "gam", "ref_vs_gam")
                    , .fns = ~round(.x))) %>% 
      mutate(across(.cols = 1:last_col()
                    , .fns = ~ifelse(is.na(.x) | is.infinite(.x), NA_real_, .x))) %>% 
      dplyr::select(1:4, 8:9, everything()) %>% 
      formattable(x = ., f2)
    )
  }
  
  if(table_type == "feature_imp"){
    insight_data$summary_stats$feature_imp %>% 
      formattable(x = .)
  }
  
}

