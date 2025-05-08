# Use cumulative heat units to estimate cotton growth stages by station and date range


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  filename = "azmet-shiny-template.html",
  
  pageFluid = bslib::page_fluid(
    title = NULL,
    theme = theme, # `scr##_theme.R`
    
    bslib::layout_sidebar(
      sidebar = sidebar, # `scr##_sidebar.R`
      
      shiny::htmlOutput(outputId = "navsetCardTabTitle"),
      
      #navsetCardTab, # `scr##_navsetCardTab.R`
      shiny::htmlOutput(outputId = "navsetCardTab"),
      
      
      #shiny::htmlOutput(outputId = "figureSummary"),
      #shiny::htmlOutput(outputId = "figureHelpText"),
      #shiny::plotOutput(outputId = "figure"),
      #plotly::plotlyOutput(outputId = "figure"),
      #shiny::htmlOutput(outputId = "figureFooter")
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
  #shinyjs::useShinyjs(html = TRUE)
  #shinyjs::hideElement("navsetCardTab")
  
  
  # Observables -----
  
  #shiny::observeEvent(dataMerge(), {
  #  shinyjs::showElement("navsetCardTab")
  #})
  
  
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

    #Calls 'fxn_dataELT()' and 'fxn_dataHeatSum()'
    fxn_dataELT(
      azmetStation = input$azmetStation,
      timeStep = "Daily",
      startDate = apiStartDate,
      endDate = apiEndDate
    )
  })
  
  ectFigure <- shiny::eventReactive(dataELT(), {
    fxn_ectFigure(
      inData = dataELT()
    )
  })

  # ectFigureFooter <- shiny::eventReactive(dataMerge(), {
  #   fxn_ectFigureFooter(
  #     azmetStation = input$azmetStation,
  #     startDate = input$plantingDate, 
  #     endDate = input$endDate
  #   )
  # })
  # 
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
  
  
  # Outputs -----
  
  output$ectFigure <- plotly::renderPlotly({
    ectFigure()
  })

  # output$ectFigureFooter <- shiny::renderUI({
  #   ectFigureFooter()
  # })
  # 
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
}

# Run --------------------

shinyApp(ui = ui, server = server)
