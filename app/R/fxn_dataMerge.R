#' fxn_dataMerge: downloads and merges individual-year data based on user-defined station
#' 
#' @param: azmetStation - AZMet station selection by user
#' @return: data_dataMerge - merged data tables from individual years


fxn_dataMerge <- function(azmetStation) {
  azmetStationStartDate <- lubridate::as_date("2021-01-01") # Placeholder for station start date
  startDate <- seasonStartDate
  endDate <- seasonEndDate
  
  while (startDate >= azmetStationStartDate) {
    dataETL <- fxn_dataETL(
      azmetStation = azmetStation, 
      timeStep = "Daily", 
      startDate = startDate, 
      endDate = endDate
    )
    
    # For case of empty data return
    if (nrow(dataETL) == 0) {
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    } else {
      if (exists("dataMerge") == FALSE) {
        dataMerge <- dataETL
      } else {
        dataMerge <- rbind(dataMerge, dataETL)
      }
      
      startDate <- min(seq(startDate, length = 2, by = "-1 year"))
      endDate <- min(seq(endDate, length = 2, by = "-1 year"))
    }
  }
  
  return(dataMerge)
}
