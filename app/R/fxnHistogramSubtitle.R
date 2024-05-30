#' `fxnHistogramSubtitle.R` - Build subtitle for histogram based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `histogramSubtitle` Subtitle for histogram based on selected station


fxnHistogramSubtitle <- function(azmetStation, inData) {
  histogramSubtitle <- 
    htmltools::h5(
      htmltools::HTML(
        "Frequency of Cotton Heat Stress Levels"
      ),
        
      class = "histogram-subtitle"
    )
  
  return(histogramSubtitle)
}
