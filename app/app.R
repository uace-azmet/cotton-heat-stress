

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
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput("figureSubtitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, plotOutput("figure"))
      ),
      
      #br(),
      #fluidRow(
      #  column(width = 11, align = "left", offset = 1, htmlOutput("figCaption"))
      #)
      
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
    id <- showNotification(ui = "Retrieving heat stress data . . .", action = NULL, duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    
    # Calls 'fxnAZMetDataELT()' and returns tidy data over multiple years
    fxnAZMetDataMerge(azmetStation = input$azmetStation)
  })
  
  #figCaption <- eventReactive(input$calculate, {
  #  fxnFigCaption(station = input$station)
  #})
  
  figure <- eventReactive(input$viewHeatStressData, {
    dataFigure <- dataAZMetDataMerge()
    fxnFigure(inData = dataFigure, azmetStation = input$azmetStation)
    #fxnFigure(inData = dataAZMetDataMerge(), station = input$azmetStation) ???
  })
  
  # Build figure footer
  #figureFooter <- eventReactive(dataAZMetDataMerge(), {
  figureFooter <- eventReactive(input$viewHeatStressData, {
    fxnFigureFooter(timeStep = "Daily")
  })
  
  # Build figure footer help text
  #figureFooterHelpText <- eventReactive(dataAZMetDataMerge(), {
  figureFooterHelpText <- eventReactive(input$viewHeatStressData, {
    fxnFigureFooterHelpText()
  })
  
  # Build figure subtitle
  figureSubtitle <- eventReactive(input$viewHeatStressData, {
    fxnFigureSubtitle(azmetStation = input$azmetStation)
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
    fxnFigureTitle()
  })
  
  # Outputs -----
  
  #output$figCaption <- renderUI(
  #  figCaption()
  #)
  
  output$figure <- renderPlot({
    figure()
  }, res = 96)
  
  output$figureFooter <- renderUI({
    figureFooter()
  })
  
  output$figureFooterHelpText <- renderUI({
    figureFooterHelpText()
  })
  
  output$figureSubtitle <- renderUI(
    figureSubtitle()
  )
  
  output$figureTitle <- renderUI(
    figureTitle()
  )
}

# Run --------------------

shinyApp(ui = ui, server = server)
