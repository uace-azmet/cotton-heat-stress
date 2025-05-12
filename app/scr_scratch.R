#source("/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/office/cotton-heat-stress/app/R/fxn_dataELT.R")
source("/Users/jeremy/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/home/cotton-heat-stress/app/R/fxn_dataELT.R")
#source("/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/laptop/cotton-heat-stress/app/R/fxn_dataELT.R")

#source("/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/office/cotton-heat-stress/app/R/fxn_dataETL_HS.R")
#source("/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/office/cotton-heat-stress/app/R/fxn_dataMerge_HS.R")
#source("/Users/jeremy/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/home/cotton-heat-stress/app/R/fxn_dataETL_HS.R")
#source("/Users/jeremy/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/home/cotton-heat-stress/app/R/fxn_dataMerge_HS.R")

azmetStations <-
  vroom::vroom(
    #file = "/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/office/cotton-heat-stress/app/aux-files/azmet-stations-api-db.csv",
    file = "/Users/jeremy/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/home/cotton-heat-stress/app/aux-files/azmet-stations-api-db.csv",
    #file = "/Users/jlweiss/Library/CloudStorage/OneDrive-UniversityofArizona/Documents/azmet/code/laptop/cotton-heat-stress/app/aux-files/azmet-stations-api-db.csv",
    delim = ",",
    col_names = TRUE,
    show_col_types = FALSE
  )

azmetStation <- "Maricopa"

# Cotton heat stress season start date
# if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-04-28"))) {
#   seasonStartDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-01-01"))
# } else {
#   seasonStartDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-01-01"))
# }
seasonStartDate <- as.Date("2021-01-01")

# Cotton heat stress season end date
# if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-04-28"))) {
#   seasonEndDate <- as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-10-08"))
# } else {
#   seasonEndDate <- as.Date(paste0(lubridate::year(Sys.Date()), "-10-08"))
# }
seasonEndDate <- lubridate::today(tzone = "America/Phoenix") - 1

library(magrittr)

inData <- fxn_dataELT(
  azmetStation = azmetStation,
  timeStep = "Daily",
  startDate = seasonStartDate,
    #dplyr::filter(azmetStations, stationName == azmetStation)$stationStartDate,
  endDate = seasonEndDate
    #dplyr::filter(azmetStations, stationName == azmetStation)$stationEndDate
) %>% 
  dplyr::mutate(datetime = lubridate::ymd(datetime))


fxn_ectFigure <- function(azmetStation, inData) {
  dataStats <- inData %>% 
    dplyr::group_by(date_doy) %>% 
    dplyr::summarize(
      max = max(heatstress_cotton_meanF, na.rm = TRUE), 
      mean = mean(heatstress_cotton_meanF, na.rm = TRUE), 
      min = min(heatstress_cotton_meanF, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(meta_station_name = azmetStation) %>% # Not needed in app version
    dplyr::mutate(heatstress_categories_max = dplyr::if_else(
      max > 86.0, "Level 2 heat stress", dplyr::if_else(
        max < 82.4, "no heat stress", "Level 1 heat stress"
      )
    )) %>%
    dplyr::mutate(heatstress_categories_mean = dplyr::if_else(
      mean > 86.0, "Level 2 heat stress", dplyr::if_else(
        mean < 82.4, "no heat stress", "Level 1 heat stress"
      )
    )) %>%
    dplyr::mutate(heatstress_categories_min = dplyr::if_else(
      min > 86.0, "Level 2 heat stress", dplyr::if_else(
        min < 82.4, "no heat stress", "Level 1 heat stress"
      )
    )) %>% 
    dplyr::mutate(
      heatstress_categories_max = factor(
        heatstress_categories_max, 
        levels = c("Level 2 heat stress", "Level 1 heat stress", "no heat stress")
      )
    ) %>% 
    dplyr::mutate(
      heatstress_categories_mean = factor(
        heatstress_categories_mean, 
        levels = c("Level 2 heat stress", "Level 1 heat stress", "no heat stress")
      )
    ) %>% 
    dplyr::mutate(
      heatstress_categories_min = factor(
        heatstress_categories_min, 
        levels = c("Level 2 heat stress", "Level 1 heat stress", "no heat stress")
      )
    ) %>% 
    dplyr::mutate(
      pseudo_date = as.Date(date_doy, origin = paste0((max(inData$date_year) - 1), "-12-31"))
    ) %>% 
    dplyr::filter(
      dplyr::case_when(
        lubridate::leap_year(max(inData$date_year)) == FALSE ~ date_doy < 366
      )
    )
  
  dataCurrentYear <- inData %>%
    dplyr::filter(date_year == max(date_year)) %>%
    dplyr::group_by(date_year)
  
  ectFigure <-
    plotly::plot_ly( # Ribbon for `dataStats` day-of-year minimum
      data = dataStats,
      x = ~pseudo_date,
      y = ~min,
      type = "scatter",
      mode = "lines",
      line = list(color = "transparent"),
      name = "day-of-year minimum",
      hoverinfo = "none",
      # text = ~paste0(
      #   "<br><b>Day-of-year Minimum:</b>  ", min, " °F",
      #   "<br><b>Heat Stress Level:</b>  ", heatstress_categories_min
      # ),
      showlegend = FALSE
    ) %>% 
    
    plotly::add_trace( # Ribbon for `dataStats` day-of-year maximum
      inherit = TRUE,
      y = ~max,
      #type = "scatter",
      #mode = "lines",
      #line = list(color = "transparent"),
      fill = "tonexty",
      fillcolor = "rgba(227, 227, 227, 1.0)",
      name = "Day-of-year Range",
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Day-of-year Maximum:</b>  ", format(round(max, digits = 1), nsmall = 1), " °F",
        "<br><b>Heat Stress Level:</b>  ", heatstress_categories_max,
        "<br><b>Day-of-year Minimum:</b>  ", format(round(min, digits = 1), nsmall = 1), " °F",
        "<br><b>Heat Stress Level:</b>  ", heatstress_categories_min
      ),
      showlegend = TRUE,
      legendgroup = "dataStats-range"
    ) %>% 
    
    plotly::add_trace( # Lines for `dataStats` day-of-year mean
      inherit = TRUE,
      # data = dataStats,
      # x = ~date_doy,
      y = ~mean,
      # type = "scatter",
      mode = "lines+markers",
      marker = list(
        color = "rgba(166, 166, 166, 1.0)",
        size = 2
      ),
      line = list(
        color = "rgba(166, 166, 166, 1.0)",
        #dash = "dot",
        #shape = "linear",
        width = 1.0
      ),
      name = "Day-of-year Average",
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>Day-of-year Average:</b>  ", format(round(mean, digits = 1), nsmall = 1), " °F",
        "<br><b>Heat Stress Level:</b>  ", heatstress_categories_mean
      ),
      showlegend = TRUE,
      legendgroup = "dataStats-mean"
    ) %>%
    
    plotly::add_trace( # Lines and points for `dataCurrentYear`
      inherit = FALSE,
      data = dataCurrentYear,
      x = ~datetime,
      y = ~heatstress_cotton_meanF,
      type = "scatter",
      mode = "lines+markers",
      #color = ~meta_station_name,
      marker = list(
        color = "rgba(25, 25, 25, 1.0)",
        size = 2
      ),
      line = list(
        color = "rgba(25, 25, 25, 1.0)",
        width = 1.0
      ),
      name = ~date_year,
      hoverinfo = "text",
      text = ~paste0(
        "<br><b>AZMet Station:</b>  ", meta_station_name,
        "<br><b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Estimated Canopy Temperature:</b>  ", format(round(heatstress_cotton_meanF, digits = 1), nsmall = 1), " °F",
        "<br><b>Heat Stress Level:</b>  ", heatstress_categories
      ),
      showlegend = TRUE,
      legendgroup = NULL
    ) %>%

    plotly::config(
      displaylogo = FALSE,
      displayModeBar = TRUE,
      #modeBarButtonsToAdd = c("togglespikelines"),
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
      annotations = list(
        list( # No Heat Stress
          align = "left",
          font = list(
            #color = "#a6a6a6",
            #color = "#8B0015",
            color = "#3b3b3b",
            family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
            size = 14
          ),
          showarrow = FALSE,
          text = "NO HEAT STRESS",
          x = 0,
          xanchor = "left",
          xref = "paper",
          xshift = 24,
          y = 78.8,
          yanchor = "bottom",
          yref = "y"
        ),
        list( # Level 1 Heat Stress
          align = "left",
          font = list(
            #color = "#8B0015",
            color = "#3b3b3b",
            family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
            size = 14
          ),
          showarrow = FALSE,
          text = "LEVEL 1 HEAT STRESS",
          x = 0,
          xanchor = "left",
          xref = "paper",
          xshift = 24,
          y = 82.4,
          yanchor = "bottom",
          yref = "y"
        ),
        list( # Level 2 Heat Stress
          align = "left",
          font = list(
            #color = "#8B0015",
            color = "#3b3b3b",
            family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
            size = 14
          ),
          showarrow = FALSE,
          text = "LEVEL 2 HEAT STRESS",
          x = 0,
          xanchor = "left",
          xref = "paper",
          xshift = 24,
          y = 86.0,
          yanchor = "bottom",
          yref = "y"
        )
      ),
      font = list(
        color = "#191919",
        family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
        size = 13
      ),
      hoverlabel = list(
        bordercolor = "transparent",
        font = list(
          color = "#191919",
          family = "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"",
          size = 14
        )
      ),
      hovermode = "x unified",
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
        pad = 3
      ),
      modebar = list(
        bgcolor = "#FFFFFF",
        orientation = "v"
      ),
      shapes = 
        list(
          list(
            fillcolor = "#8B0015",
            layer = "below",
            line = list(width = 0),
            opacity = 0.25,
            showlegend = FALSE,
            type = "rect",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 82.4, # Level 1 Heat Stress min
            y1 = 86.0, # Level 1 Heat Stress max
            yref = "y"
          ),
          list(
            fillcolor = "#8B0015",
            layer = "below",
            line = list(width = 0),
            opacity = 0.5,
            showlegend = FALSE,
            type = "rect",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 86.0, # Level 2 Heat Stress min
            y1 = yMaxLevel2, # Level 2 Heat Stress max
            yref = "y"
          )
        ),
      xaxis = list(
        range = list(min(dataStats$pseudo_date) - 0.5, max(dataStats$pseudo_date) + 0.5),
        spikecolor = "#a6a6a6",
        spikedash = "dot",
        spikemode = "across+marker",
        spikesnap = "hovered data",
        spikethickness = "-2",
        tickformat = format("%b %e"),
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Month and Day"
        ),
        zeroline = FALSE
      ),
      yaxis = list(
        range = list(
          min(dataStats$min, na.rm = TRUE) - 0.5, 
          max(dataStats$max, na.rm = TRUE) + 0.5
        ),
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Estimated Canopy Temperature (°F)"
        ),
        zeroline = FALSE
      )
    )

  return(ectFigure)
}

fxn_ectFigure(azmetStation = azmetStation, inData = inData)


# https://community.plotly.com/t/unified-hovermode-with-sublots/37606
fxn_slfFigure <- function(azmetStation, inData) {
  
  
  # Variables ----------
  
  dataCountsAllYears <- inData %>% 
    dplyr::group_by(date_doy, heatstress_categories, .drop = FALSE) %>% 
    dplyr::summarize(count = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    reshape2::dcast(date_doy ~ heatstress_categories, value.var = "count") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      perc_freqNone = (None / sum(None, `Level 1`, `Level 2`)) * 100,
      perc_freqL1 = (`Level 1` / sum(None, `Level 1`, `Level 2`)) * 100,
      perc_freqL2 = (`Level 2` / sum(None, `Level 1`, `Level 2`)) * 100
    ) %>% 
    dplyr::mutate(
      pseudo_date = 
        as.Date(
          date_doy,
          # https://stackoverflow.com/questions/24200014/convert-day-of-year-to-date
          origin = paste0((max(inData$date_year) - 1), "-12-31") # DOY zero-based
        )
    ) %>% 
    dplyr::filter(
      dplyr::case_when(
        lubridate::leap_year(max(inData$date_year)) == FALSE ~ date_doy < 366
      )
    )
  
  dataCountsCurrentYear <- inData %>% 
    dplyr::filter(date_year == max(date_year, na.rm = TRUE)) %>% 
    dplyr::group_by(date_doy, heatstress_categories, .drop = FALSE) %>% 
    dplyr::summarize(count = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    reshape2::dcast(date_doy ~ heatstress_categories, value.var = "count") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      perc_freqNone = (None / sum(None, `Level 1`, `Level 2`)) * 100,
      perc_freqL1 = (`Level 1` / sum(None, `Level 1`, `Level 2`)) * 100,
      perc_freqL2 = (`Level 2` / sum(None, `Level 1`, `Level 2`)) * 100
    ) %>% 
    dplyr::mutate(
      pseudo_date = 
        as.Date(
          date_doy,
          # https://stackoverflow.com/questions/24200014/convert-day-of-year-to-date
          origin = paste0((max(inData$date_year) - 1), "-12-31") # DOY zero-based
        )
    )
  
  layoutFontFamily <- "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  
  # Graphs ----------
  
  # Level 2 heat stress -----
  
  level2 <- 
    plotly::plot_ly(
      data = dataCountsAllYears,
      x = ~pseudo_date,
      y = ~`Level 2`,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      legendgroup = paste(
        min(inData$date_year, na.rm = TRUE),
        max(inData$date_year, na.rm = TRUE),
        sep = "-"
      ),
      line = list(
        color = "rgba(166, 166, 166, 1.0)",
        shape = "hvh", # https://plotly.com/python/line-charts/
        width = 1.0
      ),
      marker = list(
        color = "rgba(166, 166, 166, 1.0)",
        size = 2
      ),
      name = paste(
        min(inData$date_year, na.rm = TRUE),
        max(inData$date_year, na.rm = TRUE),
        sep = "-"
      ),
      showlegend = TRUE,
      text = ~paste0(
        "<b>Day-of-year:</b>  ", date_doy,
        "<br><b>Day-of-year Frequency:</b>  ", `Level 2`,
        "<br><b>Day-of-year Percent Frequency:</b>  ", format(round(perc_freqL2, digits = 1), nsmall = 1), " %"
      )
    ) %>% 
    plotly::add_trace(
      inherit = TRUE,
      data = dataCountsCurrentYear,
      hoverinfo = "text",
      legendgroup = max(inData$date_year, na.rm = TRUE),
      line = list(
        color = "rgba(25, 25, 25, 1.0)",
        shape = "hvh", # https://plotly.com/python/line-charts/
        width = 1.0
      ),
      marker = list(
        color = "rgba(25, 25, 25, 1.0)",
        size = 2
      ),
      name = max(inData$date_year, na.rm = TRUE),
      showlegend = TRUE,
      text = ~paste0(
        "<b>Date:</b>  ", gsub(" 0", " ", format(pseudo_date, "%b %d, %Y")),
        "<br><b>Frequency:</b>  ", `Level 2`
      )
    )
  
  # Level 1 heat stress -----
  
  level1 <- 
    plotly::plot_ly(
      data = dataCountsAllYears,
      x = ~pseudo_date,
      y = ~`Level 1`,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      legendgroup = paste(
        min(inData$date_year, na.rm = TRUE),
        max(inData$date_year, na.rm = TRUE),
        sep = "-"
      ),
      line = list(
        color = "rgba(166, 166, 166, 1.0)",
        shape = "hvh", # https://plotly.com/python/line-charts/
        width = 1.0
      ),
      marker = list(
        color = "rgba(166, 166, 166, 1.0)",
        size = 2
      ),
      name = paste(
        min(inData$date_year, na.rm = TRUE),
        max(inData$date_year, na.rm = TRUE),
        sep = "-"
      ),
      showlegend = FALSE,
      text = ~paste0(
        "<b>Day-of-year:</b>  ", date_doy,
        "<br><b>Day-of-year Frequency:</b>  ", `Level 1`,
        "<br><b>Day-of-year Percent Frequency:</b>  ", format(round(perc_freqL1, digits = 1), nsmall = 1), " %"
      )
    ) %>% 
    plotly::add_trace(
      inherit = TRUE,
      data = dataCountsCurrentYear,
      hoverinfo = "text",
      legendgroup = max(inData$date_year, na.rm = TRUE),
      line = list(
        color = "rgba(25, 25, 25, 1.0)",
        shape = "hvh", # https://plotly.com/python/line-charts/
        width = 1.0
      ),
      marker = list(
        color = "rgba(25, 25, 25, 1.0)",
        size = 2
      ),
      name = max(inData$date_year, na.rm = TRUE),
      showlegend = FALSE,
      text = ~paste0(
        "<b>Date:</b>  ", gsub(" 0", " ", format(pseudo_date, "%b %d, %Y")),
        "<br><b>Frequency:</b>  ", `Level 1`
      )
    )
  
  # No heat stress -----
  
  none <- 
    plotly::plot_ly(
      data = dataCountsAllYears,
      x = ~pseudo_date,
      y = ~`None`,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      legendgroup = paste(
        min(inData$date_year, na.rm = TRUE),
        max(inData$date_year, na.rm = TRUE),
        sep = "-"
      ),
      line = list(
        color = "rgba(166, 166, 166, 1.0)",
        shape = "hvh", # https://plotly.com/python/line-charts/
        width = 1.0
      ),
      marker = list(
        color = "rgba(166, 166, 166, 1.0)",
        size = 2
      ),
      name = paste(
        min(inData$date_year, na.rm = TRUE),
        max(inData$date_year, na.rm = TRUE),
        sep = "-"
      ),
      showlegend = FALSE,
      text = ~paste0(
        "<b>Day-of-year:</b>  ", date_doy,
        "<br><b>Day-of-year Frequency:</b>  ", `None`,
        "<br><b>Day-of-year Percent Frequency:</b>  ", format(round(perc_freqNone, digits = 1), nsmall = 1), " %"
      )
    ) %>% 
    plotly::add_trace(
      inherit = TRUE,
      data = dataCountsCurrentYear,
      hoverinfo = "text",
      legendgroup = max(inData$date_year, na.rm = TRUE),
      line = list(
        color = "rgba(25, 25, 25, 1.0)",
        shape = "hvh", # https://plotly.com/python/line-charts/
        width = 1.0
      ),
      marker = list(
        color = "rgba(25, 25, 25, 1.0)",
        size = 2
      ),
      name = max(inData$date_year, na.rm = TRUE),
      showlegend = FALSE,
      text = ~paste0(
        "<b>Date:</b>  ", gsub(" 0", " ", format(pseudo_date, "%b %d, %Y")),
        "<br><b>Frequency:</b>  ", `None`
      )
    )
  
  # Stacked subplots -----
  
  slfFigure <- 
    plotly::subplot(
      level2, 
      level1, 
      none,
      #heights = c(1/3, 1/3, 1/3),
      margin = 0.04,
      nrows = 3, 
      shareX = TRUE,
      shareY = TRUE,
      titleX = FALSE,
      titleY = FALSE,
      widths = 1
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
        filename = "AZMet-cotton-heat-stress",
        height = 400,
        width = 700,
        scale = 5
      )
    ) %>%
    
    plotly::layout(
      annotations = list(
        list( # No Heat Stress
          align = "left",
          font = list(
            color = "#3b3b3b",
            family = layoutFontFamily,
            size = 12
          ),
          showarrow = FALSE,
          text = "LEVEL 2 HEAT STRESS",
          x = 0,
          xanchor = "left",
          xref = "paper",
          xshift = 24,
          y = max(dataCountsAllYears$None, na.rm = TRUE),
          yanchor = "top",
          yref = "y1"  
        ),
        list( # No Heat Stress
          align = "left",
          font = list(
            color = "#3b3b3b",
            family = layoutFontFamily,
            size = 12
          ),
          showarrow = FALSE,
          text = "LEVEL 1 HEAT STRESS",
          x = 0,
          xanchor = "left",
          xref = "paper",
          xshift = 24,
          y = max(dataCountsAllYears$None, na.rm = TRUE),
          yanchor = "top",
          yref = "y2"  
        ),
        list( # No Heat Stress
          align = "left",
          font = list(
            color = "#3b3b3b",
            family = layoutFontFamily,
            size = 12
          ),
          showarrow = FALSE,
          text = "NO HEAT STRESS",
          x = 0,
          xanchor = "left",
          xref = "paper",
          xshift = 24,
          y = max(dataCountsAllYears$None, na.rm = TRUE),
          yanchor = "top",
          yref = "y3"  
        )
      ),
      font = list(
        color = "#191919",
        family = layoutFontFamily,
        size = 13
      ),
      hoverdistance = 1,
      hoverlabel = list(
        bgcolor = "rgba(255, 255, 255, 0.75)",
        bordercolor = "transparent",
        font = list(
          color = "#191919",
          family = layoutFontFamily,
          size = 14
        )
      ),
      hovermode = "x unified",
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
        pad = 3
      ),
      modebar = list(
        bgcolor = "#FFFFFF",
        orientation = "v"
      ),
      spikedistance = 1,
      xaxis = list(
        range = list(
          min(dataCountsAllYears$pseudo_date) - 0.5, 
          max(dataCountsAllYears$pseudo_date) + 0.5
        ),
        spikecolor = "#a6a6a6",
        spikedash = "dot",
        spikemode = "across+marker",
        spikesnap = "hovered data",
        spikethickness = "-2",
        tickformat = format("%b %e"),
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Month and Day"
        ),
        zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        range = list(
          0 - 0.1, 
          max(dataCountsAllYears$None, na.rm = TRUE) + 0.1
        ),
        zeroline = TRUE,
        zerolinecolor = "#eee",
        zerolinewidth = 0.5
      ),
      yaxis2 = list(
        fixedrange = TRUE,
        range = list(
          0 - 0.1, 
          max(dataCountsAllYears$None, na.rm = TRUE) + 0.1
        ),
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Number of Times in Level (Frequency)"
        ),
        zeroline = TRUE,
        zerolinecolor = "#eee",
        zerolinewidth = 0.5
      ),
      yaxis3 = list(
        fixedrange = TRUE,
        range = list(
          0 - 0.1, 
          max(dataCountsAllYears$None, na.rm = TRUE) + 0.1
        ),
        zeroline = TRUE,
        zerolinecolor = "#eee",
        zerolinewidth = 0.5
      )
    )
  
  return(slfFigure)
}

fxn_slfFigure(azmetStation = azmetStation, inData = inData)

