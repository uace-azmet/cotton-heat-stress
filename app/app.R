# Use cumulative heat units to estimate cotton growth stages by station and date range


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  filename = "azmet-shiny-template.html",
  
  pageFluid = bslib::page_fluid(
    title = NULL,
    theme = theme, # `scr##_theme.R`
    
    bslib::layout_sidebar(
      sidebar = sidebar, # `scr##_sidebar.R`
      
      shiny::htmlOutput(outputId = "figureTitle"),
      
      #navsetCardTab, # `scr##_navsetCardTab.R`
      
      shiny::htmlOutput(outputId = "figureSummary"),
      shiny::htmlOutput(outputId = "figureHelpText"),
      #shiny::plotOutput(outputId = "figure"),
      plotly::plotlyOutput(outputId = "figure"),
      shiny::htmlOutput(outputId = "figureFooter")
    ) |>
      htmltools::tagAppendAttributes(
        #https://getbootstrap.com/docs/5.0/utilities/api/
        class = "border-0 rounded-0 shadow-none"
      ),
    
    shiny::htmlOutput(outputId = "pageSupportText")
  )
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Observables -----
  
  shiny::observeEvent(input$retrieveData, {
    if (input$plantingDate > input$endDate) {
      shiny::showModal(datepickerErrorModal) # `scr##_datepickerErrorModal.R`
    }
  })
  
  
  # Reactives -----
  
  dataMerge <- shiny::eventReactive(input$retrieveData, {
    idRetrievingHeatStressData <- shiny::showNotification(
      ui = "Retrieving heat stress data . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idRetrievingHeatStressData",
      type = "message"
    )
    
    on.exit(
      removeNotification(id = idRetrievingHeatStressData), 
      add = TRUE
    )
    
    #Calls 'fxn_dataELT()' and 'fxn_dataHeatSum()'
    fxn_dataMerge(
      azmetStation = input$azmetStation, 
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  figure <- shiny::eventReactive(dataMerge(), {
    fxn_figure(
      inData = dataMerge(),
      azmetStation = input$azmetStation
    )
  })
  
  figureFooter <- shiny::eventReactive(dataMerge(), {
    fxn_figureFooter(
      azmetStation = input$azmetStation,
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  figureHelpText <- shiny::eventReactive(dataMerge(), {
    fxn_figureHelpText()
  })
  
  figureSummary <- shiny::eventReactive(dataMerge(), {
    fxn_figureSummary(
      azmetStation = input$azmetStation, 
      inData = dataMerge(),
      startDate = input$plantingDate, 
      endDate = input$endDate
    )
  })
  
  figureTitle <- shiny::eventReactive(dataMerge(), {
    fxn_figureTitle(azmetStation = input$azmetStation)
  })
  
  pageSupportText <- shiny::eventReactive(dataMerge(), {
    fxn_pageSupportText(
      timeStep = "Daily"
    )
  })
  
  
  # Outputs -----
  
  output$figure <- plotly::renderPlotly({
    figure()
  })
  
  output$pageSupportText <- shiny::renderUI({
    pageSupportText()
  })
  
  output$figureFooter <- shiny::renderUI({
    figureFooter()
  })
  
  output$figureHelpText <- shiny::renderUI({
    figureHelpText()
  })
  
  output$figureSummary <- shiny::renderUI({
    figureSummary()
  })
  
  output$figureTitle <- shiny::renderUI({
    figureTitle()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
