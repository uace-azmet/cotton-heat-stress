#' fxnAZMetDataMerge: downloads and merges individual-year data based on user-defined station
#' 
#' @param: azmetStation - AZMet station selection by user
#' @return: dataAZMetDataMerge - merged data tables from individual years


fxnAZMetDataMerge <- function(azmetStation) {
  azmetStationStartDate <- lubridate::as_date("2021-01-01") # Placeholder for station start date
  startDate <- seasonStartDate
  endDate <- seasonEndDate
  
  while (startDate >= azmetStationStartDate) {
    dataAZMetDataELT <- fxnAZMetDataELT(azmetStation = azmetStation, timeStep = "Daily", startDate = startDate, endDate = endDate)
    
    # For case of empty data return
    if (nrow(dataAZMetDataELT) == 0) {
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    } else {
      if (exists("dataAZMetDataMerge") == FALSE) {
        dataAZMetDataMerge <- dataAZMetDataELT
      } else {
        dataAZMetDataMerge <- rbind(dataAZMetDataMerge, dataAZMetDataELT)
      }
      
      startDate <- min(seq(start, length = 2, by = "-1 year"))
      endDate <- min(seq(end, length = 2, by = "-1 year"))
    }
  }
  
  return(dataAZMetDataMerge)
}
