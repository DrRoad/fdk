parameter <- get_default_hyperpar()
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"
library(plotly)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)
library(formattable)
library(shinyscreenshot)


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(id = "tabs"
                , tabPanel(title = "ForeSight"
                           , fluidRow(
                           column(width = 9, useShinyjs(),
                                    div(id = "results",
                                      fluidRow(
                                        box(width = 12, withSpinner(plotlyOutput("plot_1"), size = 1, type = 8)) # main graph
                                      )
                                      , fluidRow(
                                        box(withSpinner(plotlyOutput("plot_2", height = 250), size = 1, type = 8), width = 5)
                                        , box(withSpinner(plotlyOutput("plot_3", height = 250), size = 1, type = 8), width = 4)
                                        , box(withSpinner(formattableOutput('cum_diff', height = 250), size = 1, type = 8), width = 3)
                                      )
                                      , fluidRow(
                                        box(withSpinner(formattableOutput('year_agg', height = 100), size = 1, type = 8), width = 12)
                                        )
                                      )
                                    ) # end graph col
                           , column(width = 3, useShinyjs()
                                    , tabBox(width = 12
                                           #, useShinyjs()
                                           , tabPanel(title = "Model parameters"
                                                      , selectInput(inputId = "exc_features", label = "Excluded features"
                                                                    , choices = c("month_seas")
                                                                    , selected = NULL
                                                                    , multiple = TRUE
                                                                    , selectize = TRUE)
                                                      , dateInput("date_init"
                                                                  , label = paste0("Initial date:")
                                                                  , value = as.Date(NA))
                                                      , fluidRow(
                                                        column(
                                                          numericInput(inputId = "trend_decay"
                                                                       , label = "Trend decay"
                                                                       , value = .75
                                                                       , min = 0.1
                                                                       , max = 1
                                                                       , step = .05), width = 6)
                                                        , column(
                                                          numericInput(inputId = "k", label = "Smooth basis"
                                                                       , min = -1, max = 1e5
                                                                       , value = -1, step = 1), width = 6)
                                                        , width = 3)
                                                      , p("Options")
                                                      , radioButtons("link_function"
                                                                     , label = "Link function"
                                                                     , choices = list("Gaussian" = 1
                                                                                      , "Poisson" = 2
                                                                     ), 
                                                                     selected = 1)
                                                      , checkboxInput(inputId = "robust_mse", label = "Robust")
                                           )
                                           , tabPanel(title = "Graph options"
                                                      , checkboxInput(inputId = "gam_forecast", label = "Show GAM forecast", value = TRUE)
                                                      , checkboxInput(inputId = 'min_limit_zero', label = "Low limit zero")
                                                      )
                                           )
                                    , box(width = 12, 
                                      fluidRow(
                                        column(width = 12
                                               , selectizeInput(inputId = "key"
                                                                , label = "Key"
                                                                , choices = NULL
                                                                , multiple = FALSE
                                                                , options = list(placeholder = "Select key"
                                                                                 , onInitialize = I('function() { this.setValue(""); }'))
                                               )
                                        )
                                      )
                                      , fluidRow(width = 3
                                                 , column(actionButton("execute", "Execute"), width = 6)
                                                 , column(actionButton("reset", "Reset"), width = 6)
                                                 )
                                      )
                                    , column(width = 12
                                          , fluidRow(
                                            column(width = 12, actionButton(inputId = "screenshot", label = "Screenshot"))
                                            )
                                          )
                                    ) # end side col
                           )
                           )
                )
    )
  )
                           
                           
                        

server <- function(session, input, output) {
  
  updateSelectizeInput(session, 'key', choices = forecast_item_list, server = TRUE, selected = NULL)
  
  insight_data <- eventReactive(input$execute
                                , {
                                  parameter_int <- fdk:::react_gam_par(parameter = get_default_hyperpar()
                                                                 , values_list = list(k = input$k
                                                                                      , exc_features = input$exc_features
                                                                                      , trend_decay = input$trend_decay
                                                                                      , link_function = input$link_function))
                                  tmp <- get_insight_data(oc_data = oc_data
                                                          , key = input$key
                                                          , parameter = parameter_int)
                                  updateDateInput(session = session, inputId = "date_init"
                                                  , label = "Initial date:"
                                                  , value = min(tmp$gam_fitted$date_var)
                                                  , min = min(tmp$gam_fitted$date_var)
                                                  , max = max(tmp$gam_fitted$date_var))
                                  
                                  tmp <- fdk:::mod_stat_data(insight_data = tmp, date = input$date_init)
                                  tmp
                                })
  
  observeEvent(eventExpr = input$reset
               , {
                 reset("exc_features")
                 reset("date_init")
                 reset("k")
                 reset("trend_decay")
               })
  
  output$plot_1 <- renderPlotly({
    get_graph_stat(insight_data = insight_data(), graph_type = "forecast"
                   , conf = list(min_limit_zero = input$min_limit_zero
                                 , gam_forecast = input$gam_forecast))
  })
  output$plot_2 <- renderPlotly({
    get_graph_stat(insight_data = insight_data(), graph_type = "seas_me")
  })
  output$plot_3 <- renderPlotly({
    get_graph_stat(insight_data = insight_data(), graph_type = "derivative")
  })
  
  output$cum_diff <- renderFormattable(
    {
      tmp <- insight_data()
      fdk:::get_tables(tmp, table_type = "cum_diff")
    }
  )
  
  output$year_agg <- renderFormattable(
    {
      tmp <- insight_data()
      fdk:::get_tables(tmp, table_type = "year_agg")
    }
  )
  
  observeEvent(input$screenshot
               , {
                 screenshot(id = "results")
               }
  )
  
}

shinyApp(ui, server)