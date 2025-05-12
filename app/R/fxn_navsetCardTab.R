#' `fxn_navsetCardTab.R` - Render collection of navigation panels into a container
#' 
#' @return `navsetCardTab` - Container for collection of navigation panels


fxn_navsetCardTab <- function() {
  navsetCardTab <- bslib::navset_card_tab(
    id = "navsetCardTab",
    selected = "estimatedCanopyTemperatures",
    title = NULL,
    sidebar = NULL,
    header = NULL,
    footer = NULL,
    height = 800,
    full_screen = TRUE,
    #wrapper = card_body,
    
    bslib::nav_panel(
      title = "Estimated Canopy Temperatures",
      value = "estimatedCanopyTemperatures",
      shiny::htmlOutput(outputId = "ectFigureSummary"),
      shiny::htmlOutput(outputId = "ectFigureHelpText"),
      plotly::plotlyOutput(outputId = "ectFigure"),
      shiny::htmlOutput(outputId = "ectFigureFooter")
    ),
    
    bslib::nav_panel(
      title = "Stress-level Frequency",
      value = "stressLevelFrequency",
      # shiny::htmlOutput(outputId = "ectFigureSummary"),
      # shiny::htmlOutput(outputId = "ectFigureHelpText"),
      plotly::plotlyOutput(outputId = "slfFigure")
      # shiny::htmlOutput(outputId = "ectFigureFooter")
    )
  ) |>
    htmltools::tagAppendAttributes(
      #https://getbootstrap.com/docs/5.0/utilities/api/
      class = "border-0 rounded-0 shadow-none"
    )
  
  return(navsetCardTab)
}


