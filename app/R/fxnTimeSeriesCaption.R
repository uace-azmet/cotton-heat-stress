#' `fxnTimeSeriesCaption.R` - Build caption for time series based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `timeSeriesCaption` Caption for time series based on selected station


fxnTimeSeriesCaption <- function(azmetStation, inData) {
  
  # Adjust day-of-year values for leap years to correctly line up month and day values on plot
  for (yr in unique(inData$date_year)) {
    if (lubridate::leap_year(yr) == TRUE) {
      inData$date_doy[which(inData$date_year == yr)] <- 
        inData$date_doy[which(inData$date_year == yr)] - 1
    } else {
      inData$date_doy[which(inData$date_year == yr)] <- 
        inData$date_doy[which(inData$date_year == yr)]
    }
  }
  
  # Calculate average estimated canopy temperature for day of year of most recent date in data
  doyMostRecentDate <- inData$date_doy[which(inData$datetime == max(inData$datetime))]
  
  doyAverage <- 
    round(
      mean(
        dplyr::filter(inData, date_doy == doyMostRecentDate)$heatstress_cotton_meanF, 
        na.rm = TRUE
      ), 
      digits = 1
    )
  
  doyMinYear <- min(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  doyMaxYear <- max(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  
 # Build caption text
  timeSeriesCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "The average estimated canopy temperature on ", format(as.Date(max(inData$datetime)), "%B"), " ", format(as.Date(max(inData$datetime)), "%d"), " (vertical dotted line) at the AZMet ", azmetStation, " station is ", doyAverage, " °F, based on data from ", doyMinYear, " through ", doyMaxYear, " (gray and black points)."
        ),
      ),
      
      class = "time-series-caption"
    )
  
  return(timeSeriesCaption)
}
