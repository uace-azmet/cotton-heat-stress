# View time series and statistics of cotton heat stress by station during the growing season


# UI --------------------

ui <- 
  htmltools::htmlTemplate(
    
    filename = "azmet-shiny-template.html",
    
    pageFluid = 
      bslib::page_fluid(
        title = NULL,
        theme = theme, # `scr##_theme.R`
        
        bslib::layout_sidebar(
          sidebar = sidebar, # `scr##_sidebar.R`
          
          shiny::htmlOutput(outputId = "navsetCardTabTitle"),
          shiny::htmlOutput(outputId = "navsetCardTab")
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
  
  azDaily <- 
    shiny::eventReactive(input$retrieveData, {
      idRetrievingHeatStressData <- 
        shiny::showNotification(
          ui = "Retrieving heat stress data . . .",
          action = NULL,
          duration = NULL,
          closeButton = FALSE,
          id = "idRetrievingHeatStressData",
          type = "message"
        )
  
      on.exit(shiny::removeNotification(id = idRetrievingHeatStressData), add = TRUE)
  
      fxn_azDaily(
        azmetStation = input$azmetStation,
        startDate = 
          dplyr::filter(
            azmetStationMetadata, 
            meta_station_name == input$azmetStation
          ) |>
          dplyr::pull(start_date),
        endDate = 
          dplyr::filter(
            azmetStationMetadata, 
            meta_station_name == input$azmetStation
          ) |>
          dplyr::pull(end_date)
      )
    })
  
  ectFigure <- 
    shiny::eventReactive(azDaily(), {
      fxn_ectFigure(inData = azDaily())
    })

  ectFigureFooter <- 
    shiny::eventReactive(azDaily(), {
      fxn_ectFigureFooter(
        azmetStation = input$azmetStation,
        startDate = 
          dplyr::filter(
            azmetStationMetadata, 
            meta_station_name == input$azmetStation
          ) |>
          dplyr::pull(start_date),
        endDate = 
          dplyr::filter(
            azmetStationMetadata, 
            meta_station_name == input$azmetStation
          ) |>
          dplyr::pull(end_date)
      )
    })

  ectFigureSummary <- 
    shiny::eventReactive(azDaily(), {
      fxn_ectFigureSummary(
        azmetStation = input$azmetStation,
        inData = azDaily()
      )
    })
  
  navsetCardTab <- 
    shiny::eventReactive(azDaily(), {
      fxn_navsetCardTab()
    })
  
  navsetCardTabTitle <- 
    shiny::eventReactive(azDaily(), {
      fxn_navsetCardTabTitle(azmetStation = input$azmetStation)
    })
  
  pageSupportText <- 
    shiny::eventReactive(azDaily(), {
      fxn_pageSupportText()
    })
  
  slfFigure <- 
    shiny::eventReactive(azDaily(), {
      fxn_slfFigure(inData = azDaily())
    })
  
  slfFigureFooter <- 
    shiny::eventReactive(azDaily(), {
      fxn_slfFigureFooter(
        azmetStation = input$azmetStation,
        startDate = 
          dplyr::filter(
            azmetStationMetadata, 
            meta_station_name == input$azmetStation
          ) |>
          dplyr::pull(start_date),
        endDate = 
          dplyr::filter(
            azmetStationMetadata, 
            meta_station_name == input$azmetStation
          ) |>
          dplyr::pull(end_date)
      )
    })
  
  slfFigureSummary <- 
    shiny::eventReactive(azDaily(), {
      fxn_slfFigureSummary(
        azmetStation = input$azmetStation,
        inData = azDaily()
      )
    })
  
  
  # Outputs -----
  
  output$ectFigure <- 
    plotly::renderPlotly({
      ectFigure()
    })

  output$ectFigureFooter <- 
    shiny::renderUI({
      ectFigureFooter()
    })

  output$ectFigureSummary <- 
    shiny::renderUI({
      ectFigureSummary()
    })
  
  output$pageSupportText <- 
    shiny::renderUI({
      pageSupportText()
    })

  output$navsetCardTab <- 
    shiny::renderUI({
      navsetCardTab()
    })
  
  output$navsetCardTabTitle <- 
    shiny::renderUI({
      navsetCardTabTitle()
    })
  
  output$slfFigure <- 
    plotly::renderPlotly({
      slfFigure()
    })
  
  output$slfFigureFooter <- 
    shiny::renderUI({
      slfFigureFooter()
    })
  
  output$slfFigureSummary <- 
    shiny::renderUI({
      slfFigureSummary()
    })
}


# Run --------------------


shinyApp(ui = ui, server = server)
