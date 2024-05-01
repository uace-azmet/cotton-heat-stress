#' `fxnFigureTitle.R` - Build title for figure based on user input
#' 
#' @return `figureTitle` - Title for figure based on selected station


fxnFigureTitle <- function() {
  figureTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste(
          "Cotton Heat Stress Levels and Estimated Canopy Temperatures (°F)",
          sep = " "
        ),
      ),
        
      class = "figure-title"
    )
  
  return(figureTitle)
}
