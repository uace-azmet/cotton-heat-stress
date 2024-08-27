# View time series and statistics of cotton heat stress by station during the growing season

# Libraries
library(azmetr)
library(dplyr)
library(english)
library(ggplot2)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

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
          choices = stationNames[order(stationNames$stationName), ]$stationName,
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
  dataAZMetDataMerge <- eventReactive(input$viewHeatStressData, {
  # User feedback
    id <- showNotification(
      ui = "Retrieving heat stress data . . .", 
      action = NULL, duration = NULL, closeButton = FALSE, type = "message"
    )
    on.exit(removeNotification(id), add = TRUE)
    
    # Calls 'fxnAZMetDataELT()' and returns tidy data over multiple years
    fxnAZMetDataMerge(azmetStation = input$azmetStation)
  })
  
  # Build figure footer
  figureFooter <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooter(timeStep = "Daily")
  })
  
  # Build footer help text
  figureFooterHelpText <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureSubtitle(
      azmetStation = input$azmetStation, 
      inData = dataAZMetDataMerge()
    )
  })
  
  # Build figure title
  figureTitle <- eventReactive(dataAZMetDataMerge(), {
    fxnFigureTitle(inData = dataAZMetDataMerge())
  })
  
  # Build histogram
  histogram <- eventReactive(dataAZMetDataMerge(), {
    fxnHistogram(inData = dataAZMetDataMerge())
  })
  
  # Build histogram caption
  histogramCaption <- eventReactive(dataAZMetDataMerge(), {
    fxnHistogramCaption(
      azmetStation = input$azmetStation, 
      inData = dataAZMetDataMerge()
    )
  })
  
  # Build histogram subtitle
  histogramSubtitle <- eventReactive(dataAZMetDataMerge(), {
    fxnHistogramSubtitle()
  })
  
  # Build time series
  timeSeries <- eventReactive(dataAZMetDataMerge(), {
    fxnTimeSeries(azmetStation = input$azmetStation, inData = dataAZMetDataMerge())
  })
  
  # Build time series caption
  timeSeriesCaption <- eventReactive(dataAZMetDataMerge(), {
    fxnTimeSeriesCaption(
      azmetStation = input$azmetStation, 
      inData = dataAZMetDataMerge()
    )
  })
  
  # Build time series subtitle
  timeSeriesSubtitle <- eventReactive(dataAZMetDataMerge(), {
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
