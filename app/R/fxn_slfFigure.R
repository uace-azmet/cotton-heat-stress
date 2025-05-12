#' `fxn_slfFigure.R` generates Plotly line graph of estimated canopy temperature based on selected station
#' 
#' @param inData - data table from `fxn_dataETL()`
#' @return `slfFigure` - Plotly line graph of estimated canopy temperature based on selected station

# https://plotly-r.com/ 
# https://plotly.com/r/
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_slfFigure <- function(inData) {
  
  
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
  
  
  # Stacked subplots ----------
  
  slfFigure <- 
    plotly::subplot(
      level2, 
      level1, 
      none,
      #heights = c(1/3, 1/3, 1/3),
      margin = 0.05,
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
