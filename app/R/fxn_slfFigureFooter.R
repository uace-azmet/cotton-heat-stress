#' `fxn_slfFigureFooter.R` - Build footer for figure of stress-level frequencies based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param startDate - Start date of station record
#' @param endDate - End date of station record
#' @return `slfFigureFooter` Caption for figure of stress-level frequencies based on selected station


fxn_slfFigureFooter <- function(azmetStation, startDate, endDate) {
  slfFigureFooter <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Day-of-year frequency and relative frequency statistics for the AZMet ", azmetStation, " station are based on data from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), ". Data in the new AZMet database currently start on ", gsub(" 0", " ", format(apiStartDate, "%B %d, %Y")), "."
        )
      ), 
      
      class = "slf-figure-footer"
    )
  
  return(slfFigureFooter)
}
