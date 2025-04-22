#' `fxn_figureSummary.R` - Build summary of figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table of seasonal heat accumulation values by year
#' @param startDate - Planting date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureSummary` - Summary of figure based on user inputs


fxn_figureSummary <- function(azmetStation, inData, startDate, endDate) {
  currentYear <- lubridate::year(endDate)
  currentYearHeatSum <- 
    dplyr::filter(inData, dateYear == currentYear)$heatSum
  
  if (currentYearHeatSum < 650) {
    growthStageText <- "before Pinhead Square"
  } else if (currentYearHeatSum >= 650 & currentYearHeatSum < 750) {
    growthStageText <- "near Pinhead Square"
  } else if (currentYearHeatSum >= 750 & currentYearHeatSum < 1150) {
    growthStageText <- "between Pinhead Square and First Flower"
  } else if (currentYearHeatSum >= 1150 & currentYearHeatSum < 1250) {
    growthStageText <- "near First Flower"
  } else if (currentYearHeatSum >= 1250 & currentYearHeatSum < 1450) {
    growthStageText <- "between First Flower and One-inch Boll"
  } else if (currentYearHeatSum >= 1450 & currentYearHeatSum < 1550) {
    growthStageText <- "near One-inch Boll"
  } else if (currentYearHeatSum >= 1550 & currentYearHeatSum < 1750) {
    growthStageText <- "between One-inch Boll and Peak Bloom (Short)"
  } else if (currentYearHeatSum >= 1750 & currentYearHeatSum < 1850) {
    growthStageText <- "near Peak Bloom (Short)"
  } else if (currentYearHeatSum >= 1850 & currentYearHeatSum < 2150) {
    growthStageText <- "between Peak Bloom (Short) and Peak Bloom (Long)"
  } else if (currentYearHeatSum >= 2150 & currentYearHeatSum < 2250) {
    growthStageText <- "near Peak Bloom (Long)"
  } else if (currentYearHeatSum >= 2250 & currentYearHeatSum < 2350) {
    growthStageText <- "between Peak Bloom (Long) and Cutout (Short)"
  } else if (currentYearHeatSum >= 2350 & currentYearHeatSum < 2450) {
    growthStageText <- "near Cutout (Short)"
  } else if (currentYearHeatSum >= 2450 & currentYearHeatSum < 2750) {
    growthStageText <- "between Cutout (Short) and Cutout (Long)"
  } else if (currentYearHeatSum >= 2750 & currentYearHeatSum < 2850) {
    growthStageText <- "near Cutout (Long)"
  } else if (currentYearHeatSum >= 2850 & currentYearHeatSum < 2950) {
    growthStageText <- "between Cutout (Long) and Terminate (Short)"
  } else if (currentYearHeatSum >= 2950 & currentYearHeatSum < 3050) {
    growthStageText <- "near Terminate (Short)"
  } else if (currentYearHeatSum >= 3050 & currentYearHeatSum < 3350) {
    growthStageText <- "between Terminate (Short) and Terminate (Long)"
  } else if (currentYearHeatSum >= 3350 & currentYearHeatSum < 3450) {
    growthStageText <- "near Terminate (Long)"
  } else { # currentYearHeatSum >= 3450
    growthStageText <- "past Terminate (Long)"
  }
  
  figureSummary <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Heat unit accumulation at the AZMet ", azmetStation, " station from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), " is <b>", format(round(currentYearHeatSum, digits = 1), nsmall = 1), " degree days Fahrenheit</b>. This suggests a cotton growth stage ", "<b>", growthStageText, "</b>."
        ),
      ),
      
      class = "figure-summary"
    )
  
  return(figureSummary)
}
