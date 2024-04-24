#' fxnFigCaption: builds caption for figure
#' 
#' @param: station - AZMet station input from sidebar panel
#' @return: figCaption - custom figure caption with information from sidebar panel input
#' 
fxnFigCaption <- function(station) {
  
  urlAZMetAPI <- a(
    "api.azmet.arizona.edu", 
    href="https://api.azmet.arizona.edu/v1/observations/daily", 
    target="_blank"
  )
  
  urlBulletin <- a(
    "AZ1448 'Cotton Heat Stress'",
    href="https://staging.azmet.arizona.edu/sites/default/files/2022-07/az1448.pdf",
    target="_blank"
  )
  
  urlCottonLegacyWebpage <- a(
    "AZMet legacy website",
    href="https://cals.arizona.edu/azmet/cot-HSrpt.htm",
    target="_blank"
  )
  
  #if (Sys.Date() > as.Date(paste0(lubridate::year(Sys.Date()), "-06-15")) & station == "Yuma North Gila") {
  #  # To account for no data at Yuma North Gila station from June 16 through October 14, 2021, inclusive
  #  figCaption <- HTML(paste0(
  #    br(),
  #    "More information about cotton heat stress is in Extension bulletin", " ", urlBulletin, ".", " ", "Estimated canopy temperature and heat stress data for the Yuma North Gila station are unavailable in 2021 after June 15", ".",
  #    br(), br(), 
  #    "AZMet data are from", " ", urlAZMetAPI, ".", " ", "Estimated canopy temperatures based on recent dates may include provisional data", ".", " ", "Users of AZMet data and data applications assume all risks of its use", ".",
  #    br(), br()
  #  ))
  #} else {
  #  figCaption <- HTML(paste0(
  #    br(),
  #    "Estimated canopy temperatures below 82.4 °F indicate no cotton heat stress, between 82.4 and 86.0 °F Level 1 heat stress, and above 86.0 °F Level 2 heat stress. Daily values of estimated canopy temperatures for individual stations currently are available on the ", urlCottonLegacyWebpage, ".", "More information about cotton heat stress is in Extension bulletin", " ", urlBulletin, ".",
  #    br(), br(), 
  #    "AZMet data are from", " ", urlAZMetAPI, ".", " ", "Estimated canopy temperatures based on recent dates may include provisional data", ".", " ", "Users of AZMet data and data applications assume all risks of its use", ".",
  #    br(), br()
  #  ))
  #}
  
  figCaption <- HTML(paste0(
    br(),
    "Estimated canopy temperatures below 82.4 °F indicate no cotton heat stress, between 82.4 and 86.0 °F Level 1 heat stress, and above 86.0 °F Level 2 heat stress (vertical axis on graph). Tables of daily values of estimated canopy temperatures for individual stations currently are available on the ", urlCottonLegacyWebpage, ".", " ", "Cotton heat stress data for this webpage update from May through September. More information about cotton heat stress is in Extension bulletin", " ", urlBulletin, ".",
    br(), br(), 
    "AZMet data are from", " ", urlAZMetAPI, ".", " ", "Estimated canopy temperatures based on recent dates may include provisional data", ".", " ", "Users of AZMet data and data applications assume all risks of its use", ".",
    br(), br()
  ))
  
  return(figCaption)
}