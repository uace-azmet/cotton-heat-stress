sidebar <- 
  bslib::sidebar(
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
      bsicons::bs_icon("sliders", class = "bolder-icon"), 
      htmltools::HTML("&nbsp;<strong>DATA OPTIONS</strong>&nbsp;"),
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
      choices = azmetStationMetadata$meta_station_name,
      selected = azmetStationMetadata$meta_station_name[1]
      # choices = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name,
      # selected = azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name[1]
    ),
    
    shiny::actionButton(
      inputId = "retrieveData", 
      label = "RETRIEVE DATA",
      class = "btn btn-block btn-blue"
    )
  ) # bslib::sidebar()
