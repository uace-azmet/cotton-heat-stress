#' `fxnHistogramCaption.R` - Build caption for histogram based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `histogramCaption` Caption for histogram based on selected station


fxnHistogramCaption <- function(azmetStation, inData) {
  
  doyMostRecentDate <- inData$date_doy[which(inData$datetime == max(inData$datetime))]
  
  doyMinYear <- min(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  doyMaxYear <- max(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  
  doyMostRecentLevel <- inData$heatstress_categories[which(inData$datetime == max(inData$datetime))]
  
  doyMostRecentLevelFreq <- 
    nrow(
      dplyr::filter(
        dplyr::filter(inData, date_doy == doyMostRecentDate), 
        heatstress_categories == doyMostRecentLevel
      )
    )
  
  if (doyMostRecentLevel == "NO HEAT STRESS") {
    doyMostRecentLevel <- "no heat stress"
  } else if (doyMostRecentLevel == "LEVEL 1 HEAT STRESS") {
    doyMostRecentLevel <- "Level 1 heat stress"
  } else {
    doyMostRecentLevel <- "Level 2 heat stress"
  }
  
  if (doyMostRecentLevelFreq == 1) {
    numberOfTimes <- "time"
  } else {
    numberOfTimes <- "times"
  }
  
  histogramCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Based on data for ", gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d")), " (vertical dotted line) from ", doyMinYear, " through ", doyMaxYear, " at the AZMet ", azmetStation, " station, estimated canopy temperatures have been in ", doyMostRecentLevel, " ", doyMostRecentLevelFreq, " ", numberOfTimes, "."
        ),
      ),
      
      class = "histogram-caption"
    )
  
  return(histogramCaption)
}
