#' `fxn_ectFigureHelpText.R` - Build help text for figure of estimated canopy temperatures
#' 
#' @return `ectFigureHelpText` - Help text for figure of estimated canopy temperatures


fxn_ectFigureHelpText <- function() {
  ectFigureHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Click or tap and drag to zoom into an area of interest. Hover over data for variable values. Click or tap on legend items to toggle data visibility. Select from the icons to the right of the graph for additional functionality."
      ), 
      
      class = "ect-figure-help-text"
    )
  
  return(ectFigureHelpText)
}
