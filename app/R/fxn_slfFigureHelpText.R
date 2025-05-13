#' `fxn_slfFigureHelpText.R` - Build help text for figure of stress-level frequency
#' 
#' @return `slfFigureHelpText` - Help text for figure of stress-level frequency


fxn_slfFigureHelpText <- function() {
  slfFigureHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Click or tap and drag to zoom into an area of interest. Hover over data for variable values. Click or tap on legend items to toggle data visibility. Select from the icons to the right of the graph for additional functionality."
      ), 
      
      class = "slf-figure-help-text"
    )
  
  return(slfFigureHelpText)
}
