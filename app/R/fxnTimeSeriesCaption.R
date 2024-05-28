#' `fxnTimeSeriesCaption.R` - Build caption for time series based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `timeSeriesCaption` Caption for time series based on selected station


fxnTimeSeriesCaption <- function(azmetStation, inData) {
  timeSeriesCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "The average of estimated canopy temperatures at the AZMet ", azmetStation, " station on MONTH / DAY is VALUE °F. Values are based on data through ", gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y")), "."
        ),
      ),
      
      class = "time-series-caption"
    )
  
  return(timeSeriesCaption)
}
