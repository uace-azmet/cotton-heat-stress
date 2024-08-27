#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(inData) {
 
  doyValue <- 
    format(
      round(
        dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_cotton_meanF,
        digits = 1
      ), 
      nsmall = 1
    )
  
  figureTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste0(
          "<b>Estimated canopy temperature: ", doyValue, " °F</b>"
        ),
      ),
        
      class = "figure-title"
    )
  
  return(figureTitle)
}
