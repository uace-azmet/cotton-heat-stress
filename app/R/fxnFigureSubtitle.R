#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table of seasonal heat accumulation values by year
#' @return `figureSubtitle` - Subtitle for figure based on selected AZMet station


fxnFigureSubtitle <- function(azmetStation, inData) {
  
  doyValue <- 
    format(
      round(
        dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_cotton_meanF,
        digits = 1
      ), 
      nsmall = 1
    )
  
  if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "NO HEAT STRESS") {
    doyLevel <- "no heat stress"
  } else if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "LEVEL 1 HEAT STRESS") {
    doyLevel <- "Level 1 heat stress"
  } else {
    doyLevel <- "Level 2 heat stress"
  }
  
  figureSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "The estimated canopy temperature of ", doyValue, " °F on ", gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y")), " at the AZMet ", azmetStation, " station indicates <b>", doyLevel, "</b>."
        ),
      ),
      
      class = "figure-subtitle"
    )
  
  return(figureSubtitle)
}
