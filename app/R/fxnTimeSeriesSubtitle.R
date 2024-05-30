#' `fxnTimeSeriesSubtitle.R` - Build subtitle for time series based on user input
#' 
#' @return `timeSeriesSubtitle` Subtitle for time series based on selected station


fxnTimeSeriesSubtitle <- function() {
  timeSeriesSubtitle <- 
    htmltools::h5(
      htmltools::HTML(
        "Estimated Canopy Temperatures (°F)"
      ),
        
      class = "time-series-subtitle"
    )
  
  return(timeSeriesSubtitle)
}
