# View time series and statistics of cotton heat stress by station during the growing season


# Functions
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  "azmet-shiny-template.html",
  
  sidebarLayout = sidebarLayout(
    position = "left",
    
    sidebarPanel(
      id = "sidebarPanel",
      width = 4,
      
      verticalLayout(
        helpText(em(
          "Select an AZMet station. Then, click or tap 'VIEW HEAT STRESS DATA'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = azmetStations[order(azmetStations$stationName), ]$stationName,
          selected = "Aguila"
        ),
        
        br(),
        actionButton(
          inputId = "viewHeatStressData", 
          label = "VIEW HEAT STRESS DATA",
          class = "btn btn-block btn-blue"
        )
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("figureTitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureSubtitle"))
      ),
      
      br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("timeSeriesSubtitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput("timeSeries"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("timeSeriesCaption"))
      ),
      
      br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("histogramSubtitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput("histogram"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("histogramCaption"))
      ),
      
      br(), br(), br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooterHelpText"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "figureFooter"))
      ),
     
      br()
    ) # mainPanel()
  ) # sidebarLayout()
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
  # Download and prep AZMet data
  dataMerge <- eventReactive(input$viewHeatStressData, {
  # User feedback
    id <- showNotification(
      ui = "Retrieving heat stress data . . .", 
      action = NULL, duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(id), add = TRUE)
    
    # Calls 'fxn_dataELT()' and returns tidy data over multiple years
    fxn_dataMerge(azmetStation = input$azmetStation)
  })
  
  # Build figure footer
  figureFooter <- eventReactive(dataMerge(), {
    fxnFigureFooter(timeStep = "Daily")
  })
  
  # Build footer help text
  figureFooterHelpText <- eventReactive(dataMerge(), {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(dataMerge(), {
    fxnFigureSubtitle(
      azmetStation = input$azmetStation, 
      insData = dataMerge()
    )
  })
  
  # Build figure title
  figureTitle <- eventReactive(dataMerge(), {
    fxnFigureTitle(inData = dataMerge())
  })
  
  # Build histogram
  histogram <- eventReactive(dataMerge(), {
    fxnHistogram(inData = dataMerge())
  })
  
  # Build histogram caption
  histogramCaption <- eventReactive(dataMerge(), {
    fxnHistogramCaption(
      azmetStation = input$azmetStation, 
      inData = dataMerge()
    )
  })
  
  # Build histogram subtitle
  histogramSubtitle <- eventReactive(dataMerge(), {
    fxnHistogramSubtitle()
  })
  
  # Build time series
  timeSeries <- eventReactive(dataMerge(), {
    fxnTimeSeries(azmetStation = input$azmetStation, inData = dataMerge())
  })
  
  # Build time series caption
  timeSeriesCaption <- eventReactive(dataMerge(), {
    fxnTimeSeriesCaption(
      azmetStation = input$azmetStation, 
      inData = dataMerge()
    )
  })
  
  # Build time series subtitle
  timeSeriesSubtitle <- eventReactive(dataMerge(), {
    fxnTimeSeriesSubtitle()
  })
  
  # Outputs -----
  
  output$figureFooter <- renderUI({
    figureFooter()
  })
  
  output$figureFooterHelpText <- renderUI({
    figureFooterHelpText()
  })
  
  output$figureTitle <- renderUI(
    figureTitle()
  )
  
  output$histogram <- renderPlot({
    histogram()
  }, res = 96)
  
  output$histogramCaption <- renderUI(
    histogramCaption()
  )
  
  output$histogramSubtitle <- renderUI(
    histogramSubtitle()
  )
  
  output$figureSubtitle <- renderUI({
    figureSubtitle()
  })
  
  output$timeSeries <- renderPlot({
    timeSeries()
  }, res = 96)
  
  output$timeSeriesCaption <- renderUI(
    timeSeriesCaption()
  )
  
  output$timeSeriesSubtitle <- renderUI(
    timeSeriesSubtitle()
  )
}

# Run --------------------

shinyApp(ui = ui, server = server)
