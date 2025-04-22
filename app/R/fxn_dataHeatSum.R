#' `fxn_dataHeatSum` calculates heat unit accumulation based on user input
#' 
#' @param inData dataELT
#' @param azmetStation - AZMet station selection by user
#' @param endDate - End date of period of interest
#' @return `dataAZMetDataHeatSum` - Data table with cumulative heat units by year


fxn_dataHeatSum <- function(inData, azmetStation, endDate) {
  if (nrow(inData) == 0) { # For case of empty data return
    dataHeatSum <- data.frame(matrix(
      data = NA,
      nrow = 1, 
      ncol = length(c("meta_station_name", "heatSum", "heatSumLabel", "dateYear"))
    ))
    
    colnames(dataHeatSum) <- 
      c("meta_station_name", "heatSum", "heatSumLabel", "dateYear")
    
    dataHeatSum <- dataHeatSum %>%
      dplyr::mutate(meta_station_name = azmetStation) %>%
      dplyr::mutate(heatSum = 0.0) %>%
      dplyr::mutate(heatSumLabel = "NA") %>%
      dplyr::mutate(dateYear = lubridate::year(endDate))
  } else {
    dataHeatSum <- inData %>%
      dplyr::group_by(meta_station_name) %>%
      dplyr::summarize(heatSum = sum(heat_units_55F, na.rm = TRUE)) %>%
      dplyr::mutate(heatSumLabel = format(round(heatSum, digits = 1), nsmall = 1)) %>%
      dplyr::mutate(dateYear = lubridate::year(endDate))
  }
  
  return(dataHeatSum)
}
