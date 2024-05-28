#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function(azmetStation) {
  figureTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste(
          "Cotton Heat Stress at the AZMet", azmetStation, "Station", 
          sep = " "
        ),
      ),
        
      class = "figure-title"
    )
  
  return(figureTitle)
}
