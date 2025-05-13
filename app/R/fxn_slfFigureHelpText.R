#' `fxn_slfFigureHelpText.R` - Build help text for figure of stress level frequencies
#' 
#' @return `slfFigureHelpText` - Help text for figure of stress level frequencies


fxn_slfFigureHelpText <- function() {
  slfFigureHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Hover over data for variable values. Click or tap on legend items to toggle data visibility. Click or tap and drag to zoom into an area of interest. Select from the icons to the right of the graph for additional functionality."
      ), 
      
      class = "slf-figure-help-text"
    )
  
  return(slfFigureHelpText)
}