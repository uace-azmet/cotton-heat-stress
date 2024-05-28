# View time series and statistics of cotton heat stress by station during the growing season

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->
# 'azmet-shiny-template.html': <!-- CSS specific to this AZMet Shiny app -->

# Libraries
library(azmetr)
library(dplyr)
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
      
      br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("timeSeriesSubtitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput("timeSeries"))
      ),
      
      br(),
      
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
      
      br(),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("histogramCaption"))
      ),
      
      br(), br(),
      
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
  #figureFooter <- eventReactive(dataAZMetDataMerge(), {
  figureFooter <- eventReactive(input$viewHeatStressData, {
    fxnFigureFooter(timeStep = "Daily")
  })
  
  # Build footer help text
  #figureFooterHelpText <- eventReactive(dataAZMetDataMerge(), {
  figureFooterHelpText <- eventReactive(input$viewHeatStressData, {
    fxnFigureFooterHelpText()
  })
  
  # Build figure title
  figureTitle <- eventReactive(input$viewHeatStressData, {
    #validate(
    #  need(
    #    expr = input$plantingDate <= input$endDate, 
    #    message = "Please select a 'Planting Date' that is earlier than or the same as the 'End Date'."
    #  ),
    #  errorClass = "datepicker"
    #)
    
    #fxnFigureTitle(inData = dataAZMetDataSumHUs(), endDate = input$endDate)
    fxnFigureTitle(azmetStation = input$azmetStation)
  })
  
  # Build histogram
  histogram <- eventReactive(input$viewHeatStressData, {
    dataHistogram <- dataAZMetDataMerge()
    fxnHistogram(inData = dataHistogram, azmetStation = input$azmetStation)
    #fxnHistogram(inData = dataAZMetDataMerge(), station = input$azmetStation) ???
  })
  
  # Build histogram caption
  histogramCaption <- eventReactive(input$viewHeatStressData, {
    fxnHistogramCaption(
      azmetStation = input$azmetStation, 
      inData = dataAZMetDataMerge()
    )
  })
  
  # Build histogram subtitle
  histogramSubtitle <- eventReactive(input$viewHeatStressData, {
    fxnHistogramSubtitle(azmetStation = input$azmetStation, inData = dataAZMetDataMerge())
  })
  
  # Build time series
  timeSeries <- eventReactive(input$viewHeatStressData, {
    dataTimeSeries <- dataAZMetDataMerge()
    fxnTimeSeries(inData = dataTimeSeries, azmetStation = input$azmetStation)
    #fxnFigure(inData = dataAZMetDataMerge(), station = input$azmetStation) ???
  })
  
  # Build time series caption
  timeSeriesCaption <- eventReactive(input$viewHeatStressData, {
    fxnTimeSeriesCaption(
      azmetStation = input$azmetStation, 
      inData = dataAZMetDataMerge()
    )
  })
  
  # Build time series subtitle
  timeSeriesSubtitle <- eventReactive(input$viewHeatStressData, {
    fxnTimeSeriesSubtitle(
      azmetStation = input$azmetStation, 
      inData = dataAZMetDataMerge()
    )
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
