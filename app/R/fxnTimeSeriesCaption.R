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
  
  doyMostRecentDate <- inData$date_doy[which(inData$datetime == max(inData$datetime))]
  
  doyValue <- 
    format(
      round(
        dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_cotton_meanF,
        digits = 1
      ), 
      nsmall = 1
    )
  
  doyAverageValue <- 
    format(
      round(
        mean(
          dplyr::filter(inData, date_doy == doyMostRecentDate)$heatstress_cotton_meanF, 
          na.rm = TRUE
        ), 
        digits = 1
      ), 
      nsmall = 1
    )
  
  doyMaxValue <- format(
    round(
      max(
        dplyr::filter(inData, date_doy == doyMostRecentDate)$heatstress_cotton_meanF, 
        na.rm = TRUE
      ),
      digits = 1
    ),
    nsmall = 1
  )
  
  doyMinValue <- format(
    round(
      min(
        dplyr::filter(inData, date_doy == doyMostRecentDate)$heatstress_cotton_meanF, 
        na.rm = TRUE
      ),
      digits = 1
    ),
    nsmall = 1
  )
  
  doyMinYear <- min(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  doyMaxYear <- max(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  
  if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "NO HEAT STRESS") {
    doyLevel <- "no heat stress"
  } else if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "LEVEL 1 HEAT STRESS") {
    doyLevel <- "Level 1 heat stress"
  } else {
    doyLevel <- "Level 2 heat stress"
  }
  
  
 # Build caption text
  timeSeriesCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "The estimated canopy temperature of ", doyValue, " °F (black point) on ", gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y")), " at the AZMet ", azmetStation, " station indicates ", doyLevel, ". The maximum, average, and minimum of estimated canopy temperatures on ", gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d")), " (vertical dotted line) are ", doyMaxValue, ", ", doyAverageValue, ", and ", doyMinValue, " °F, respectively, based on data from ", doyMinYear, " through ", doyMaxYear, " (black and gray points)."
        ),
      ),
      
      class = "time-series-caption"
    )
  
  return(timeSeriesCaption)
}
