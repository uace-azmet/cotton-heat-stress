#' `fxnHistogramCaption.R` - Build caption for histogram based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `histogramCaption` Caption for histogram based on selected station


fxnHistogramCaption <- function(azmetStation, inData) {
  
  doyMostRecentDate <- 
    inData$date_doy[which(inData$datetime == max(inData$datetime))]
  
  doyMinYear <- min(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  doyMaxYear <- max(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  
  doyMostRecentLevel <- 
    inData$heatstress_categories[which(inData$datetime == max(inData$datetime))]
  
  doyMostRecentLevelFreq <- 
    nrow(
      dplyr::filter(
        dplyr::filter(inData, date_doy == doyMostRecentDate), 
        heatstress_categories == doyMostRecentLevel
      )
    )
  
  doyYearCount <- length(dplyr::filter(inData, date_doy == doyMostRecentDate)$date_year)
  
  if (doyMostRecentLevel == "NO HEAT STRESS") {
    doyMostRecentLevel <- "no heat stress"
  } else if (doyMostRecentLevel == "LEVEL 1 HEAT STRESS") {
    doyMostRecentLevel <- "Level 1 heat stress"
  } else {
    doyMostRecentLevel <- "Level 2 heat stress"
  }
  
  if (doyMostRecentLevelFreq < 10) {
    freqText <- english::as.english(doyMostRecentLevelFreq)
  } else {
    freqText <- doyMostRecentLevelFreq
  }
  
  if (doyMostRecentLevelFreq == 1) {
    numberOfTimesText <- "time"
  } else {
    numberOfTimesText <- "times"
  }
  
  if ((max(inData$date_year) - min(inData$date_year)) + 1 < 10) {
    recordLengthText <- 
      #english::as.english((max(inData$date_year) - min(inData$date_year)) + 1)
      #english::as.english(length(unique(inData$date_year)))
      english::as.english(doyYearCount)
  } else {
    #recordLengthText <- (max(inData$date_year) - min(inData$date_year)) + 1
    #recordLengthText <- length(unique(inData$date_year))
    recordLengthText <- doyYearCount
  }
  
  histogramCaption <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Based on data for ", gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d")), " (vertical dotted line) from ", doyMinYear, " through ", doyMaxYear, " at the AZMet ", azmetStation, " station, estimated canopy temperatures have been in ", doyMostRecentLevel, " ", freqText, " ", numberOfTimesText, " in ", recordLengthText, " years."
        ),
      ),
      
      class = "histogram-caption"
    )
  
  return(histogramCaption)
}
