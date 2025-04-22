sidebar <- bslib::sidebar(
  width = 300,
  position = "left",
  open = list(desktop = "always", mobile = "always-above"),
  id = "sidebar",
  title = NULL,
  bg = "#FFFFFF",
  fg = "#191919",
  class = NULL,
  max_height_mobile = NULL,
  gap = NULL,
  padding = NULL,
  
  htmltools::p(
    bsicons::bs_icon("sliders"), 
    htmltools::HTML("&nbsp;"), 
    "DATA OPTIONS"
  ),
  
  shiny::helpText(
    "Select an AZMet station and set dates for planting and the end of the period of interest. Then, click or tap 'CALCULATE HEAT UNITS'."
  ),
  
  shiny::selectInput(
    inputId = "azmetStation", 
    label = "AZMet Station",
    choices = azmetStations[order(azmetStations$stationName), ]$stationName,
    selected = "Aguila"
  ),
  
  shiny::dateInput(
    inputId = "plantingDate",
    label = "Planting Date",
    value = initialPlantingDate,
    min = "2024-01-01", # January 1 of current growing season,
    max = Sys.Date() - 1,
    format = "MM d, yyyy",
    startview = "month",
    weekstart = 0, # Sunday
    width = "100%",
    autoclose = TRUE
  ),
  
  shiny::dateInput(
    inputId = "endDate",
    label = "End Date",
    value = initialEndDate,
    min = "2024-01-01", # January 1 of current growing season,
    max = initialEndDate,
    format = "MM d, yyyy",
    startview = "month",
    weekstart = 0, # Sunday
    width = "100%",
    autoclose = TRUE
  ),
  
  shiny::actionButton(
    inputId = "calculateHeatUnits", 
    label = "CALCULATE HEAT UNITS",
    class = "btn btn-block btn-blue"
  )
) # bslib::sidebar()
