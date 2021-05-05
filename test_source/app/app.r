parameter <- get_default_hyperpar()
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    tabsetPanel(id = "tabs"
                , tabPanel(title = "GAM dynamics"
                           , fluidRow(
                             useShinyjs(), 
                             box(width = 9
                                 , dropdownButton(
                                   tags$h3("List of Input"),
                                   checkboxInput(inputId = 'min_limit_zero', label = "Minimum limit as zero"),
                                   selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
                                   sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
                                   circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                   tooltip = tooltipOptions(title = "Click to see inputs !")
                                 )
                                 , withSpinner(plotlyOutput("plot_1"), size = 1, type = 8)
                                 )
                             , tabBox(width = 3
                                   #, useShinyjs()
                                   , tabPanel(title = "Configuration"
                                              , dateInput("date_init"
                                                          , label = paste0("Initial date:")
                                                          , value = as.Date(NA))
                                              , checkboxInput(inputId = "robust_mse", label = "Robust Marginal Seasonal Effects")
                                              )
                                   , tabPanel(title = "Parameters"
                                              , selectInput(inputId = "exc_features", label = "Excluded features"
                                                            , choices = c("month_seas")
                                                            , selected = NULL
                                                            , multiple = TRUE
                                                            , selectize = TRUE)
                                              , numericInput(inputId = "k", label = "GAM smooth basis dimension"
                                                             , min = -1, max = 1e5
                                                             , value = -1, step = 1)
                                              
                                              , checkboxInput(inputId = "gam_forecast", label = "Show GAM forecast", value = TRUE)
                                              , numericInput(inputId = "trend_decay"
                                                             , label = "Trend decay"
                                                             , value = .75
                                                             , min = 0
                                                             , max = 1
                                                             , step = .05))
                                   , fluidRow(
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
                                   , fluidRow(
                                     column(actionButton("execute", "Execute"), width = 6)
                                     , column(actionButton("reset", "Reset"), width = 6)
                                     )
                                   )
                             ),
                           fluidRow(
                             box(withSpinner(plotlyOutput("plot_2", height = 300), size = 1, type = 8), width = 4)
                             , box(withSpinner(plotlyOutput("plot_3", height = 300), size = 1, type = 8), width = 3)
                             #, valueBoxOutput("diff_mean", width = 2)
                             , tableOutput('summary_stat_table')
                             
                             )
                           )
                )
  )
)

server <- function(session, input, output) {

  updateSelectizeInput(session, 'key', choices = forecast_item_list, server = TRUE, selected = NULL)
  # stat_data <- reactive({
  #   
  #   #reset(id = "data_init")
  #   return(tmp)
  # })
  
  insight_data <- eventReactive(input$execute
               , {
                 parameter_int <- react_gam_par(parameter = get_default_hyperpar()
                                            , values_list = list(k = input$k
                                                                 , exc_features = input$exc_features
                                                                 , trend_decay = input$trend_decay))
                 tmp <- get_insight_data(oc_data = oc_data
                                         , key = input$key
                                         , parameter = parameter_int)
                 updateDateInput(session = session, inputId = "date_init"
                                 , label = "Initial date:"
                                 , value = min(tmp$gam_fitted$date_var)
                                 , min = min(tmp$gam_fitted$date_var)
                                 , max = max(tmp$gam_fitted$date_var))
                 
                 tmp <- mod_stat_data(insight_data = tmp, date = input$date_init)
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
  
  output$summary_stat_table <- renderTable(
    {
      tmp <- insight_data()
      tmp$summary_stats %>% 
        rename("Months ahead" = 1, "Diff" = 2) %>% 
        mutate(Diff = paste0(Diff*100, "%"))
    }
    , width = 2
  )
  
  # output$diff_mean <- renderValueBox({
  #   
  #   text_in <- tmp$summary_stats %>%
  #     rowwise() %>%
  #     mutate(diff = paste0(months_ahead, " ("
  #                          , fit_cum_diff_perc*100, "%)")) %>%
  #     pull(diff) %>%
  #     paste0(collapse = "\n")
  #   valueBox(value = tags$p(text_in, style = "font-size: 75%;")
  #     , "GAM vs actual forecast (months ahead) \n (history vs forecast)"
  #   )})
}

shinyApp(ui, server)