#' `fxnHistogramCaption.R` - Build caption for histogram based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `histogramCaption` Caption for histogram based on selected station


fxnHistogramCaption <- function(azmetStation, inData) {
  histogramCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Frequencies are based on data through ", 
          gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y")),
          " (vertical dotted line)."
        ),
      ),
      
      class = "histogram-caption"
    )
  
  return(histogramCaption)
}
