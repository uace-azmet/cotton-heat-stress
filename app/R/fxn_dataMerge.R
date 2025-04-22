#' `fxn_dataMerge` downloads and merges individual-year data since API database start and based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `dataMerge` - merged data tables from individual years


fxn_dataMerge <- function(azmetStation, startDate, endDate) {
  azmetStationStartDate <- apiStartDate # Placeholder for station start date
  
  while (startDate >= azmetStationStartDate) {
    
    dataELT <- fxn_dataELT(
      azmetStation = azmetStation, 
      timeStep = "Daily", 
      startDate = startDate, 
      endDate = endDate
    )
    
    dataHeatSum <- fxn_dataHeatSum(
      inData = dataELT,
      azmetStation = azmetStation,
      endDate = endDate
    )
    
    if (azmetStation == "Yuma North Gila") {
      nodataDateRange <- 
        lubridate::interval(
          start = lubridate::date("2021-06-16"), 
          end = lubridate::date("2021-10-21")
        )
      
      userDateRange <- lubridate::interval(start = startDate, end = endDate)
      
      if (lubridate::int_overlaps(int1 = nodataDateRange, int2 = userDateRange) == TRUE) {
        dataHeatSum$heatSum <- 0.00
        dataHeatSum$heatSumLabel <- "NA" 
      }
    }
    
    if (exists("dataMerge") == FALSE) {
      dataMerge <- dataHeatSum
    } else {
      dataMerge <- rbind(dataMerge, dataHeatSum)
    }
    
    startDate <- min(seq(startDate, length = 2, by = "-1 year"))
    endDate <- min(seq(endDate, length = 2, by = "-1 year"))
  }
  
  return(dataMerge)
}
