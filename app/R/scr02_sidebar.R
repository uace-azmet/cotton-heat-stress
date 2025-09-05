sidebar <- bslib::sidebar(
  width = 300,
  position = "left",
  open = list(desktop = "open", mobile = "always-above"),
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
    "DATA OPTIONS",
    htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
    bslib::tooltip(
      bsicons::bs_icon("info-circle"),
      "Select an AZMet station. Then, click or tap 'RETRIEVE DATA'.",
      id = "infoDataOptions",
      placement = "right"
    ),
    
    class = "data-options-title"
  ),
  
  shiny::selectInput(
    inputId = "azmetStation", 
    label = "AZMet Station",
    choices = azmetStations[order(azmetStations$stationName), ]$stationName,
    selected = "Aguila"
  ),
  
  shiny::actionButton(
    inputId = "retrieveData", 
    label = "RETRIEVE DATA",
    class = "btn btn-block btn-blue"
  )
) # bslib::sidebar()
