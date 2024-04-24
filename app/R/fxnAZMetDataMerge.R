#' fxnAZMetDataMerge: downloads and merges individual-year data based on user-defined station
#' 
#' @param: station - AZMet station name
#' @return: dfDailyMerge - merged data tables from individual years
#' 
fxnAZMetDataMerge <- function(station) {
  start <- seasonStartDate
  end <- seasonEndDate
  
  while (start >= min(seq(seasonStartDate, length = 2, by = "-1 year"))) {
    dfDaily <- fxnAZMetDataELT(station = station, startDate = start, endDate = end)
    
    # For case of empty data return
    if (nrow(dfDaily) == 0) {
      start <- min(seq(startDate, length = 2, by = "-1 year"))
      end <- min(seq(endDate, length = 2, by = "-1 year"))
    } else {
      if (exists("dfDailyMerge") == FALSE) {
        dfDailyMerge <- dfDaily
      } else {
        dfDailyMerge <- rbind(dfDailyMerge, dfDaily)
      }
      
      start <- min(seq(start, length = 2, by = "-1 year"))
      end <- min(seq(end, length = 2, by = "-1 year"))
    }
  }
  
  return(dfDailyMerge)
}
