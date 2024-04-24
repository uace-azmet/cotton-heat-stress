#' fxnAZMetDataELT: AZMet daily data download from API-based database
#' 
#' @param: station - AZMet station name
#' @param: startDate - start date of period of interest
#' @param: endDate - end date of period of interest
#' @return: dfDaily - transformed data table
#' 
fxnAZMetDataELT <- function(station, startDate, endDate) {
  
  dfDaily <- azmetr::az_daily(
    station_id = dplyr::filter(stns, stationName == station)$stationID, 
    start_date = startDate, 
    end_date = endDate
  )
  
  # For case of empty data return
  if (nrow(dfDaily) == 0) {
    dfDaily <- data.frame(matrix(nrow = 0, ncol = length(c(varsID, varsMeasure))))
    colnames(dfDaily) <- c(varsID, varsMeasure)
  } else {
    # Tidy data
    dfDaily <- dfDaily %>%
      dplyr::select(all_of(c(varsID, varsMeasure))) %>%
      dplyr::mutate(date_doy = as.integer(.data$date_doy)) %>%
      dplyr::mutate(date_year = as.integer(.data$date_year)) %>%
      dplyr::mutate(datetime = as.character(.data$datetime)) %>%
      dplyr::mutate(heatstress_cotton_meanF = round(.data$heatstress_cotton_meanF, digits = 1))
  }
  
  return(dfDaily)
}
