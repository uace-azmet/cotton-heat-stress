#' `fxn_figureTitle.R` - Build title for figure
#' 
#' @param azmetStation - AZMet station selection by user
#' @return `figureTitle` - Title for figure based on selected station


fxn_figureTitle <- function(azmetStation) {
  figureTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            paste0(
              "Cumulative Heat Units at the AZMet ", azmetStation, " Station"
            )
          )
        ),
      ),
      
      class = "figure-title"
    )
  
  return(figureTitle)
}
