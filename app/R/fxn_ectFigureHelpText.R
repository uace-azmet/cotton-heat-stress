#' `fxn_ectFigureHelpText.R` - Build help text for figure of estimated canopy temperatures
#' 
#' @return `ectFigureHelpText` - Help text for figure of estimated canopy temperatures


fxn_ectFigureHelpText <- function() {
  ectFigureHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Hover over data for variable values and click or tap on legend items to toggle data visibility. Select from the icons to the right of the graph for additional functionality."
      ), 
      
      class = "ect-figure-help-text"
    )
  
  return(ectFigureHelpText)
}