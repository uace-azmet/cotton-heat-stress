#' `fxn_slfFigureSummary.R` - Build summary of figure for stress-level frequency based on selected station
#' 
#' @param azmetStation - AZMet station selection by user
#' @param inData - data table from `fxn_dataETL()`
#' @return `slfFigureSummary` - Summary of figure for stress-level frequency based on selected station


fxn_slfFigureSummary <- function(azmetStation, inData) {
  
  
  # Variables ----------
  
  if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "None") {
    doyLevel <- "no heat stress"
  } else if (dplyr::filter(inData, datetime == max(inData$datetime))$heatstress_categories == "Level 1") {
    doyLevel <- "Level 1 heat stress"
  } else {
    doyLevel <- "Level 2 heat stress"
  }
  
  doyMaxDate <-
    dplyr::filter(inData, datetime == max(inData$datetime))$date_doy

  doyMonthDay <- gsub(" 0", " ", format(as.Date(max(inData$datetime)), "%B %d, %Y"))
  
  dataRelFreq <- inData %>%
    dplyr::filter(date_doy == doyMaxDate) %>%
    dplyr::group_by(date_doy, heatstress_categories, .drop = FALSE) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    reshape2::dcast(date_doy ~ heatstress_categories, value.var = "count") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rel_freqNone = (None / sum(None, `Level 1`, `Level 2`)),
      rel_freqL1 = (`Level 1` / sum(None, `Level 1`, `Level 2`)),
      rel_freqL2 = (`Level 2` / sum(None, `Level 1`, `Level 2`))
    )
  
  if (doyLevel == "no heat stress") {
    doyRelFreq <- dataRelFreq$rel_freqNone
  } else if (doyLevel == "Level 1 heat stress") {
    doyRelFreq <- dataRelFreq$rel_freqL1
  } else {
    doyRelFreq <- dataRelFreq$rel_freqL2
  }
  
  
  # Summary text ----------
  
  slfFigureSummary <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Day-of-year estimated canopy temperatures for ", doyMonthDay, " at the AZMet ", azmetStation, " station indicate a relativive frequency, or empirical probability, of <b>", format(round(doyRelFreq, digits = 2), nsmall = 2), "</b> for <b>", doyLevel, "</b>."
        )
      ),
      
      class = "slf-figure-summary"
    )
  
  return(slfFigureSummary)
}
