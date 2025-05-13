# View time series and statistics of cotton heat stress by station during the growing season


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  filename = "azmet-shiny-template.html",
  
  pageFluid = bslib::page_fluid(
    title = NULL,
    theme = theme, # `scr##_theme.R`
    
    bslib::layout_sidebar(
      sidebar = sidebar, # `scr##_sidebar.R`
      
      shiny::htmlOutput(outputId = "navsetCardTabTitle"),
      shiny::htmlOutput(outputId = "navsetCardTab"),
      
      #height = "800px"
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
  
  shiny::observe({
    if (shiny::req(input$navsetCardTab) == "estimatedCanopyTemperatures") {
      #message("estimatedCanopyTemperatures has been selected")
      selectedTab <- selectedTab("estimatedCanopyTemperatures")
    } else if (shiny::req(input$navsetCardTab) == "stressLevelFrequency") {
      #message("stressLevelFrequency has been selected")
      selectedTab <- selectedTab("stressLevelFrequency")
    }
  })
  
  
  # Reactives -----
  
  dataELT <- shiny::eventReactive(input$retrieveData, {
    idRetrievingHeatStressData <- shiny::showNotification(
      ui = "Retrieving heat stress data . . .",
      action = NULL,
      duration = NULL,
      closeButton = FALSE,
      id = "idRetrievingHeatStressData",
      type = "message"
    )

    on.exit(
      shiny::removeNotification(id = idRetrievingHeatStressData),
      add = TRUE
    )

    fxn_dataELT(
      azmetStation = input$azmetStation,
      timeStep = "Daily",
      startDate = 
        dplyr::filter(azmetStations, stationName == input$azmetStation)$stationStartDate,
      endDate = 
        dplyr::filter(azmetStations, stationName == input$azmetStation)$stationEndDate
    )
  })
  
  ectFigure <- shiny::eventReactive(dataELT(), {
    fxn_ectFigure(
      inData = dataELT()
    )
  })

  ectFigureFooter <- shiny::eventReactive(dataELT(), {
    fxn_ectFigureFooter(
      azmetStation = input$azmetStation,
      startDate =
        dplyr::filter(azmetStations, stationName == input$azmetStation)$stationStartDate,
      endDate =
        dplyr::filter(azmetStations, stationName == input$azmetStation)$stationEndDate
    )
  })

  ectFigureHelpText <- shiny::eventReactive(dataELT(), {
   fxn_ectFigureHelpText()
  })

  ectFigureSummary <- shiny::eventReactive(dataELT(), {
    fxn_ectFigureSummary(
      azmetStation = input$azmetStation,
      inData = dataELT()
    )
  })
  
  navsetCardTab <- shiny::eventReactive(dataELT(), {
    fxn_navsetCardTab()
  })
  
  navsetCardTabTitle <- shiny::eventReactive(dataELT(), {
    fxn_navsetCardTabTitle(
      azmetStation = input$azmetStation
    )
  })
  
  pageSupportText <- shiny::eventReactive(dataELT(), {
    fxn_pageSupportText(
      timeStep = "Daily"
    )
  })
  
  slfFigure <- shiny::eventReactive(dataELT(), {
    fxn_slfFigure(
      inData = dataELT()
    )
  })
  
  slfFigureFooter <- shiny::eventReactive(dataELT(), {
    fxn_slfFigureFooter(
      azmetStation = input$azmetStation,
      startDate =
        dplyr::filter(azmetStations, stationName == input$azmetStation)$stationStartDate,
      endDate =
        dplyr::filter(azmetStations, stationName == input$azmetStation)$stationEndDate
    )
  })
  
  slfFigureHelpText <- shiny::eventReactive(dataELT(), {
    fxn_slfFigureHelpText()
  })
  
  slfFigureSummary <- shiny::eventReactive(dataELT(), {
    fxn_slfFigureSummary(
      azmetStation = input$azmetStation,
      inData = dataELT()
    )
  })
  
  
  # Outputs -----
  
  output$ectFigure <- plotly::renderPlotly({
    ectFigure()
  })

  output$ectFigureFooter <- shiny::renderUI({
   ectFigureFooter()
  })

  output$ectFigureHelpText <- shiny::renderUI({
   ectFigureHelpText()
  })

  output$ectFigureSummary <- shiny::renderUI({
    ectFigureSummary()
  })
  
  output$pageSupportText <- shiny::renderUI({
    pageSupportText()
  })

  output$navsetCardTab <- shiny::renderUI({
    navsetCardTab()
  })
  
  output$navsetCardTabTitle <- shiny::renderUI({
    navsetCardTabTitle()
  })
  
  output$slfFigure <- plotly::renderPlotly({
    slfFigure()
  })
  
  output$slfFigureFooter <- shiny::renderUI({
    slfFigureFooter()
  })
  
  output$slfFigureHelpText <- shiny::renderUI({
    slfFigureHelpText()
  })
  
  output$slfFigureSummary <- shiny::renderUI({
    slfFigureSummary()
  })
}

# Run --------------------

shinyApp(ui = ui, server = server)
