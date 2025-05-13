#' fxnAZMetDataMerge: downloads and merges individual-year data based on user-defined station
#' 
#' @param: azmetStation - AZMet station selection by user
#' @return: dataAZMetDataMerge - merged data tables from individual years


fxnAZMetDataMerge <- function(azmetStation) {
  azmetStationStartDate <- lubridate::as_date("2021-01-01") # Placeholder for station start date
  startDate <- seasonStartDate
  endDate <- seasonEndDate
  
  
  dataAZMetDataELT <- fxnAZMetDataELT(
    azmetStation = azmetStation, 
    timeStep = "Daily", 
    startDate = startDate, 
    endDate = endDate
  )
  
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
  }
  
  # Patch for `azmetr` error in parse_params: Please supply an `end_date` earlier than today.
  endDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-10-08"))
  
  startDate <- min(seq(startDate, length = 2, by = "-1 year"))
  endDate <- min(seq(endDate, length = 2, by = "-1 year"))
  
  
  
  while (startDate >= azmetStationStartDate) {
    dataAZMetDataELT <- fxnAZMetDataELT(
      azmetStation = azmetStation, 
      timeStep = "Daily", 
      startDate = startDate, 
      endDate = endDate
    )
    
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
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  return(dataAZMetDataMerge)
}
