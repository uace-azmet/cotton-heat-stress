source("/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/office/cotton-heat-stress/app/R/fxn_dataETL_HS.R")
source("/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/office/cotton-heat-stress/app/R/fxn_dataMerge_HS.R")

azmetStations <- 
  vroom::vroom(
    file = "/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/office/cotton-heat-stress/app/aux-files/azmet-stations-api-db.csv", 
    delim = ",", 
    col_names = TRUE, 
    show_col_types = FALSE
  )

azmetStation <- "Maricopa"

# Cotton heat stress season start date
if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-04-28"))) {
  seasonStartDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-04-24"))
} else {
  seasonStartDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-04-24"))
}

# Cotton heat stress season end date
if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-04-28"))) {
  seasonEndDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-10-08"))
} else {
  seasonEndDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-10-08"))
}

dataMerge <- fxn_dataMerge(azmetStation = azmetStation)

inData <- dataMerge

fxn_slsGraph <- function(inData) {
  inData <- inData |>
    dplyr::mutate(datetime = lubridate::ymd(datetime))
  
  dataPriorYears <- inData %>% 
    dplyr::filter(date_year != max(date_year)) %>% 
    dplyr::group_by(date_year)
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(date_year == max(date_year)) %>% 
    dplyr::group_by(date_year)
  
  slsGraph <- 
    plotly::plot_ly( # Lines and points for `dataPriorYears`
      data = dataPriorYears,
      x = ~date_doy,
      y = ~heatstress_cotton_meanF,
      type = "scatter",
      mode = "lines+markers",
      #color = "rgba(201, 201, 201, 1.0)",
      marker = list(
        color = "rgba(201, 201, 201, 1.0)",
        size = 3
      ),
      line = list(
        color = "rgba(201, 201, 201, 1.0)", 
        width = 1
      ),
      name = "prior years",
      hoverinfo = "text",
      text = ~paste0(
        #"<br><b>", stationVariable, ":</b>  ", .data[[stationVariable]],
        "<br><b>AZMet station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y"))#,
        #"<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      ),
      showlegend = TRUE,
      legendgroup = "dataPriorYears"
    ) %>% 
    
    plotly::add_trace( # Lines and points for `dataCurrentYear`
      inherit = FALSE,
      data = dataCurrentYear,
      x = ~date_doy,
      y = ~heatstress_cotton_meanF,
      type = "scatter",
      mode = "lines+markers",
      #color = ~meta_station_name,
      marker = list(
        color = "rgba(25, 25, 25, 1.0)",
        size = 3
      ),
      line = list(
        #color = ~meta_station_name, 
        width = 1.5
      ),
      name = ~date_year,
      hoverinfo = "text",
      text = ~paste0(
        #"<br><b>", stationVariable, ":</b>  ", .data[[stationVariable]],
        "<br><b>AZMet station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y"))#,
        #"<br><b>Time:</b>  ", format(datetime, "%H:%M:%S")
      ),
      showlegend = TRUE,
      legendgroup = NULL
    ) %>% 
    
    plotly::config(
      displaylogo = FALSE,
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "autoScale2d",
        "hoverClosestCartesian", 
        "hoverCompareCartesian", 
        "lasso2d",
        "select"
      ),
      scrollZoom = FALSE,
      toImageButtonOptions = list(
        format = "png", # Either png, svg, jpeg, or webp
        filename = "AZMet-data-viewer-15minute-station-level-summaries",
        height = 400,
        width = 700,
        scale = 5
      )
    ) %>%
    
    plotly::layout(
      font = list(
        color = "#191919",
        family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
        size = 13
      ),
      hoverlabel = list(
        font = list(
          family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
          size = 14
        )
      ),
      legend = list(
        orientation = "h",
        traceorder = "reversed",
        x = 0.00,
        xanchor = "left",
        xref = "container",
        y = 1.05,
        yanchor = "bottom",
        yref = "container"
      ),
      margin = list(
        l = 0,
        r = 50, # For space between plot and modebar
        b = 80, # For space between x-axis title and caption or figure help text
        t = 0,
        pad = 0
      ),
      modebar = list(
        bgcolor = "#FFFFFF",
        orientation = "v"
      ),
      xaxis = list(
        #range = list(~(min(datetime) - 3000), ~(max(datetime) + 3000)), # unix timestamp values
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Month and Day"
        ),
        zeroline = FALSE
      ),
      yaxis = list(
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Estimated Canopy Temperature (°F)"
        ),
        zeroline = FALSE
      )
    )
  
  return(slsGraph)
}

fxn_slsGraph(inData = inData)
