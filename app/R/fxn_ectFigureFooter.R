#' `fxn_ectFigureFooter.R` - Build footer for figure of estimated canopy temperatures based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param startDate - Start date of station record
#' @param endDate - End date of station record
#' @return `ectFigureFooter` Caption for figure of estimated canopy temperatures based on selected station


fxn_ectFigureFooter <- function(azmetStation, startDate, endDate) {
  ectFigureFooter <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Day-of-year average and range statistics for the AZMet ", azmetStation, " station are based on data from ", gsub(" 0", " ", format(startDate, "%B %d, %Y")), " through ", gsub(" 0", " ", format(endDate, "%B %d, %Y")), ". Data in the new AZMet database currently start on ", gsub(" 0", " ", format(apiStartDate, "%B %d, %Y")), "."
        )
      ), 
      
      class = "ect-figure-footer"
    )
  
  return(ectFigureFooter)
}
