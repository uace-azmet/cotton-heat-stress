#' `fxn_slfFigureSummary.R` - Build summary of figure for stress-level frequencies based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table from `fxn_dataETL()`
#' @return `slfFigureSummary` - Summary of figure for stress-level frequencies based on user inputs


fxn_slfFigureSummary <- function(azmetStation, inData) {
  
  
  # Variables ----------
  
  
  # doyMaxDate <- inData %>% 
  #   dplyr::filter(max(inData$datetime))$date_doy
  # 
  # dataPercFreq <- inData %>% 
  #   dplyr::filter(date_doy == doyMaxDate) %>% 
  #   dplyr::group_by(heatstress_categories, .drop = FALSE) %>% 
  #   dplyr::summarize(count = dplyr::n()) %>% 
  #   dplyr::ungroup() %>% 
  #   reshape2::dcast(date_doy ~ heatstress_categories, value.var = "count") %>% 
  #   dplyr::rowwise() %>% 
  #   dplyr::mutate(
  #     perc_freqNone = (None / sum(None, `Level 1`, `Level 2`)) * 100,
  #     perc_freqL1 = (`Level 1` / sum(None, `Level 1`, `Level 2`)) * 100,
  #     perc_freqL2 = (`Level 2` / sum(None, `Level 1`, `Level 2`)) * 100
  #   )
  
  doyMonthDay <- gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y"))
  
  if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "None") {
    doyLevel <- "no heat stress"
  } else if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "Level 1") {
    doyLevel <- "Level 1 heat stress"
  } else {
    doyLevel <- "Level 2 heat stress"
  }
  
  # Summary text ----------
  
  slfFigureSummary <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "For ", doyMonthDay, " at the AZMet ", azmetStation, " station, day-of-year estimated canopy temperatures have indicated <b>", doyLevel, "</b> in <b>", "100", " % </b> of recent years."
        )
      )
    )
  
  
  class = "slf-figure-summary"
  
  return(slfFigureSummary)
}
