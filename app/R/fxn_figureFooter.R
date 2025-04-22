#' `fxn_figureFooter.R` - Build footer for figure based on user input
#' 
#' @param azmetStation AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `figureFooter` Caption for figure based on selected station


fxn_figureFooter <- function(azmetStation, startDate, endDate) {
  azmetStationStartDate <- apiStartDate # Placeholder for station start date
  nonOperational <- 0
  standardText <- paste0("Data in the new AZMet database currently start on ", gsub(" 0", " ", format(apiStartDate, "%B %d, %Y")), ".")
  
  # Check for non-operational dates of stations given `startDate` and `endDate`
  if (azmetStation == "Yuma N.Gila") {
    nodataDateRange <- 
      lubridate::interval(
        start = lubridate::date("2021-06-16"), 
        end = lubridate::date("2021-10-21")
      )
    
    while (startDate >= azmetStationStartDate) {
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        nonOperational <- 1
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  # Generate figure footer based on presence/absence of non-operational dates
  if (azmetStation == "Yuma N.Gila" & nonOperational == 1) {
    figureFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "A missing bar indicates an overlap of requested data and the period from June 16, 2021 through October 10, 2021, when the ", azmetStation, " station was not in operation.",
            sep = " "
          )
        ),
        
        class = "figure-footer"
      )
  } else if (azmetStation == "Mohave ETo") {
    figureFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "Missing bars indicate an overlap of requested data and the period prior to July 20, 2023, when measurements begin at the ", azmetStation, " station.",
            sep = " "
          )
        ),
        
        class = "figure-footer"
      )
  } else if (azmetStation == "Wellton ETo") {
    figureFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "Missing bars indicate an overlap of requested data and the period prior to May 2, 2023, when measurements begin at the ", azmetStation, " station.",
            sep = " "
          )
        ),
        
        class = "figure-footer"
      )
  } else if (azmetStation == "Yuma Valley ETo") {
    figureFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste(
            standardText,
            "Missing bars indicate an overlap of requested data and the period prior to May 2, 2023, when measurements begin at the ", azmetStation, " station.",
            sep = " "
          )
        ),
        
        class = "figure-footer"
      )
  } else {
    figureFooter <- 
      htmltools::p(
        htmltools::HTML(standardText), 
        class = "figure-footer"
      )
  }
  
  return(figureFooter)
}
