#' `fxn_ectFigure.R` generates Plotly line graph of estimated canopy temperature based on selected station
#' 
#' @param inData - data table from `fxn_dataETL()`
#' @return `ectFigure` - Plotly line graph of estimated canopy temperature based on selected station

# https://plotly-r.com/ 
# https://plotly.com/r/
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_ectFigure <- function(inData) {
  
  
  # Variables ----------
  
  dataStats <- inData %>% 
    dplyr::group_by(date_doy) %>% 
    dplyr::summarize(
      max = max(heatstress_cotton_meanF, na.rm = TRUE), 
      mean = mean(heatstress_cotton_meanF, na.rm = TRUE), 
      min = min(heatstress_cotton_meanF, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      heatstress_categories_max = dplyr::if_else(
        max > 86.0, "Level 2", dplyr::if_else(
          max < 82.4, "None", "Level 1"
        )
      ),
      heatstress_categories_mean = dplyr::if_else(
        mean > 86.0, "Level 2", dplyr::if_else(
          mean < 82.4, "None", "Level 1"
        )
      ),
      heatstress_categories_min = dplyr::if_else(
        min > 86.0, "Level 2", dplyr::if_else(
          min < 82.4, "None", "Level 1"
        )
      ),
      heatstress_categories_max = factor(
        heatstress_categories_max, 
        levels = c("Level 2", "Level 1", "None")
      ),
      heatstress_categories_mean = factor(
        heatstress_categories_mean, 
        levels = c("Level 2", "Level 1", "None")
      ),
      heatstress_categories_min = factor(
        heatstress_categories_min, 
        levels = c("Level 2", "Level 1", "None")
      ),
      pseudoDate = 
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
  
  dataCurrentYear <- inData %>%
    dplyr::filter(date_year == max(date_year))
  
  layoutFontFamily <- "proxima-nova, calibri, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, \"Helvetica Neue\", Arial, \"Noto Sans\", sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\""
  
  if (max(dataStats$max, na.rm = TRUE) > 90) {
    yAxisMax <- max(dataStats$max, na.rm = TRUE)
  } else {
    yAxisMax <- 90
  }
  
  
  # Figure ----------
  
  ectFigure <- 
    plotly::plot_ly( # Ribbon for `dataStats` day-of-year minimum
      data = dataStats,
      x = ~pseudoDate,
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
      # type = "scatter",
      # mode = "lines",
      # line = list(color = "transparent"),
      fill = "tonexty",
      fillcolor = "rgba(227, 227, 227, 1.0)",
      name = "Day-of-year Range",
      hoverinfo = "text",
      text = ~paste0(
        "<b>Day-of-year Maximum:</b>  ", format(round(max, digits = 1), nsmall = 1), " °F",
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
        width = 1.0
      ),
      name = "Day-of-year Average",
      hoverinfo = "text",
      text = ~paste0(
        "<b>Day-of-year Average:</b>  ", format(round(mean, digits = 1), nsmall = 1), " °F",
        "<br><b>Heat Stress Level:</b>  ", heatstress_categories_mean, "<br>"
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
        "<b>Date:</b>  ", gsub(" 0", " ", format(datetime, "%b %d, %Y")),
        "<br><b>Estimated Canopy Temperature:</b>  ", format(round(heatstress_cotton_meanF, digits = 1), nsmall = 1), " °F",
        "<br><b>Heat Stress Level:</b>  ", heatstress_categories, "<br>"
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
          y = 82.4,
          yanchor = "bottom",
          yref = "y"
        ),
        list( # Level 2 Heat Stress
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
          y = 86.0,
          yanchor = "bottom",
          yref = "y"
        )
      ),
      font = list(
        color = "#191919",
        family = layoutFontFamily,
        size = 13
      ),
      hoverlabel = list(
        bgcolor = "rgba(255, 255, 255, 0.75)",
        bordercolor = "transparent",
        font = list(
          color = "#191919",
          family = layoutFontFamily,
          size = 14
        )
      ),
      hoverdistance = 1,
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
            y1 = 120, # Level 2 Heat Stress max
            yref = "y"
          )
        ),
      spikedistance = 1,
      xaxis = list(
        range = list(
          min(dataStats$pseudoDate) - 0.5, 
          max(dataStats$pseudoDate) + 0.5
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
          min(dataStats$min, na.rm = TRUE) - 0.5,
          yAxisMax + 0.5
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
