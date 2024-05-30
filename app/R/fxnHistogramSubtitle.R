#' `fxnHistogramSubtitle.R` - Build subtitle for histogram based on user input
#' 
#' @return `histogramSubtitle` Subtitle for histogram based on selected station


fxnHistogramSubtitle <- function() {
  histogramSubtitle <- 
    htmltools::h5(
      htmltools::HTML(
        "Frequency of Cotton Heat Stress Levels"
      ),
        
      class = "histogram-subtitle"
    )
  
  return(histogramSubtitle)
}
