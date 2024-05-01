#' `fxnFigureSubtitle.R` - Build subtitle for figure based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @return `figureSubtitle` Subtitle for figure based on selected station


fxnFigureSubtitle <- function(azmetStation) {
  figureSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "at the", azmetStation, "station",
          sep = " "
        ),
      ),
        
      class = "figure-subtitle"
    )
  
  return(figureSubtitle)
}
