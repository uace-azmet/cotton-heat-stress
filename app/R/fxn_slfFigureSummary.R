#' `fxn_ectFigureSummary.R` - Build summary of figure for estimated canopy temperatures based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table from `fxn_dataETL()`
#' @return `ectFigureSummary` - Summary of figure for estimated canopy temperatures based on user inputs


fxn_ectFigureSummary <- function(azmetStation, inData) {
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
  
  ectFigureSummary <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "The estimated canopy temperature of <b>", doyValue, " °F</b> on ", doyMonthDay, " at the AZMet ", azmetStation, " station indicates <b>", doyLevel, "</b>."
        ),
      ),
      
      class = "ect-figure-summary"
    )
  
  return(ectFigureSummary)
}
