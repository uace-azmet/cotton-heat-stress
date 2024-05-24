#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `figureSubtitle` Subtitle for figure based on selected station


fxnFigureSubtitle <- function(azmetStation, inData) {
  figureSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "at the AZMet", azmetStation, "station", "with data through ", 
          gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y")), "\n",
          sep = " "
        ),
      ),
        
      class = "figure-subtitle"
    )
  
  return(figureSubtitle)
}
