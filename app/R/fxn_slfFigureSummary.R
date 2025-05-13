#' `fxn_slfFigureSummary.R` - Build summary of figure for stress-level frequencies based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table from `fxn_dataETL()`
#' @return `slfFigureSummary` - Summary of figure for stress-level frequencies based on user inputs


fxn_slfFigureSummary <- function(azmetStation, inData) {
  doyValue <-
    format(
      round(
        dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_cotton_meanF,
        digits = 1
      ),
      nsmall = 1
    )
  
  if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "None") {
    doyLevel <- "no heat stress"
  } else if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "Level 1") {
    doyLevel <- "Level 1 heat stress"
  } else {
    doyLevel <- "Level 2 heat stress"
  }
  
  doyMonthDay <- gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y"))
  
  slfFigureSummary <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "The estimated canopy temperature of <b>", doyValue, " °F</b> on ", doyMonthDay, " at the AZMet ", azmetStation, " station indicates <b>", doyLevel, "</b>."
        ),
      ),
      
      class = "slf-figure-summary"
    )
  
  return(slfFigureSummary)
}
