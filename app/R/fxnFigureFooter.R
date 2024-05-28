#' `fxnFigureFooter.R` - Build footer for figure
#' 
#' @param timeStep - AZMet data time step
#' @return `figureFooter` - Footer for figure


fxnFigureFooter <- function(timeStep) {
  # Inputs
  apiURL <- a(
    "api.azmet.arizona.edu", 
    href="https://api.azmet.arizona.edu/v1/observations/daily", # Daily data
    target="_blank"
  )
  
  azmetrURL <- a(
    "azmetr", 
    href="https://uace-azmet.github.io/azmetr/",
    target="_blank"
  )
  
  bulletinURL <- a(
    "AZ1448 'Cotton Heat Stress'",
    href="https://staging.azmet.arizona.edu/sites/default/files/2022-07/az1448.pdf",
    target="_blank"
  )
  
  cottonWebpageURL <- a(
    "cotton-related information",
    href="https://azmet.arizona.edu/application-areas/cotton",
    target="_blank"
  )
  
  todayDate <- gsub(" 0", " ", format(lubridate::today(), "%B %d, %Y"))
  
  todayYear <- lubridate::year(lubridate::today())
  
  webpageCode <- a(
    "GitHub page", 
    href="https://github.com/uace-azmet/cotton-heat-stress", 
    target="_blank"
  )
  
  webpageDataVariables <- a(
    "data variables", 
    href="https://azmet.arizona.edu/about/data-variables", 
    target="_blank"
  )
  
  webpageNetworkMap <- a(
    "station locations", 
    href="https://azmet.arizona.edu/about/network-map", 
    target="_blank"
  )
  
  webpageStationMetadata <- a(
    "station metadata", 
    href="https://azmet.arizona.edu/about/station-metadata", 
    target="_blank"
  )
  
  webpageAZMet <- a(
    "AZMet website", 
    href="https://azmet.arizona.edu/", 
    target="_blank"
  )
  
  # Footer text
  figureFooter <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Estimated canopy temperatures below 82.4 °F indicate no cotton heat stress, between 82.4 and 86.0 °F Level 1 heat stress, and above 86.0 °F Level 2 heat stress (vertical axis on graph). Tables of daily values of estimated canopy temperatures and stress levels for individual stations and the current growing season currently are available from the AZMet webpage with ", cottonWebpageURL, ".", " ", "More information about cotton heat stress is in Extension bulletin", " ", bulletinURL, ".",
          br(), br(), 
          timeStep, " ", "AZMet data are from", " ", apiURL, " and accessed using the ", azmetrURL, " R package. Values from recent dates may be based on provisional data", ".", " ", "More information about", " ", webpageDataVariables, ",", " ", webpageNetworkMap, ",", " ", "and", " ", webpageStationMetadata, " ", "is available on the", " ", webpageAZMet, ".", " ", "Users of AZMet data and related information assume all risks of its use", ".",
          br(), br(),
          "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https://azmet.arizona.edu. Accessed ", todayDate, "', along with 'Arizona Meteorological Network (", todayYear, ") Cotton Heat Stress. https://viz.datascience.arizona.edu/azmet/cotton-heat-stress. Accessed ", todayDate, "'.",
          br(), br(),
          "For information on how this webpage is put together, please visit the", " ", webpageCode, " ", "for this tool."
        )
      ),
      
      class = "figure-footer"
    )
  
  return(figureFooter)
}
