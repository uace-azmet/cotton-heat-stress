#' `fxnTimeSeriesSubtitle.R` - Build subtitle for time series based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `timeSeriesSubtitle` Subtitle for time series based on selected station


fxnTimeSeriesSubtitle <- function(azmetStation, inData) {
  timeSeriesSubtitle <- 
    htmltools::h5(
      htmltools::HTML(
        "Estimated Canopy Temperatures (°F)"
      ),
        
      class = "time-series-subtitle"
    )
  
  return(timeSeriesSubtitle)
}
