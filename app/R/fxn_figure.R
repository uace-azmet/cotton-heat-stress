#' `fxn_figure` generates bar chart of cumulative heat units of current and recent years with cotton growth stage labels
#' 
#' @param inData - data table of seasonal heat accumulation values by year
#' @param azmetStation - user-specified AZMet station
#' @return `figure` - png of figure

# https://plotly-r.com/ 
# https://plotly.com/r/reference/ 
# https://plotly.github.io/schema-viewer/
# https://github.com/plotly/plotly.js/blob/c1ef6911da054f3b16a7abe8fb2d56019988ba14/src/components/fx/hover.js#L1596
# https://www.color-hex.com/color-palette/1041718


fxn_figure <- function(inData, azmetStation) {
  
  # Inputs -----
  
  dataCurrentYear <- inData %>% 
    dplyr::filter(
      dateYear == lubridate::year(lubridate::today(tz = "America/Phoenix"))
    ) %>% 
    dplyr::mutate(dateYear = as.factor(dateYear))
  
  dataOtherYears <- inData %>% 
    dplyr::filter(
      dateYear != lubridate::year(lubridate::today(tz = "America/Phoenix"))
    ) %>% 
    dplyr::mutate(dateYear = as.factor(dateYear))
  
  
  # Figure -----
  
  figure <- 
    plotly::plot_ly( # Bars for `dataOtherYears`
      data = dataOtherYears,
      x = ~dateYear,
      y = ~heatSum,
      marker = list(color = "#989898"),
      name = "other years",
      showlegend = FALSE,
      hoverinfo = "text",
      hovertext = ~paste0(
        "<br><b>AZMet station:</b>  ", azmetStation,
        "<br><b>Year:</b>  ", dateYear,
        "<br><b>Cumulative Heat Units:</b>  ", format(round(heatSum, digits = 1), nsmall = 1)
      ),
      type = "bar",
      yaxis = "y1"
    ) %>% 
    
    plotly::add_trace( # Bar for `dataCurrentYear`
      inherit = FALSE,
      data = dataCurrentYear,
      x = ~dateYear,
      y = ~heatSum,
      marker = list(color = "#191919"),
      name = "current year",
      showlegend = FALSE,
      hoverinfo = "text",
      hovertext = ~paste0(
        "<br><b>AZMet station:</b>  ", azmetStation,
        "<br><b>Year:</b>  ", dateYear,
        "<br><b>Cumulative Heat Units:</b>  ", format(round(heatSum, digits = 1), nsmall = 1)
      ),
      type = "bar",
      yaxis = "y2"
    ) %>%
    
    plotly::config(
      displaylogo = FALSE,
      displayModeBar = FALSE,
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
        filename = "AZMet-cotton-growth-stages-and-heat-units",
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
      margin = list(
        l = 0,
        r = 200, # For space between plot and modebar
        b = 0,
        t = 10, # For space to show `3400` tick
        pad = 3 # For space between gridlines and yaxis labels
      ),
      modebar = list(
        bgcolor = "#FFFFFF",
        orientation = "v"
      ),
      shapes = 
        list(
          list(
            type = "rect",
            fillcolor = "#c9c9c9",
            line = list(width = 0),
            layer = "below",
            opacity = 0.5,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 1800, # Peak Bloom (Short)
            y1 = 2200, # Peak Bloom (Long)
            yref = "y"
          ),
          list(
            type = "rect",
            fillcolor = "#c9c9c9",
            line = list(width = 0),
            layer = "below",
            opacity = 0.5,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 2400, # Cutout (Short)
            y1 = 2800, # Cutout (Long)
            yref = "y"
          ),
          list(
            type = "rect",
            fillcolor = "#c9c9c9",
            line = list(width = 0),
            layer = "below",
            opacity = 0.5,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 3000, # Terminate (Short)
            y1 = 3400, # Terminate (Long)
            yref = "y"
          ),
          list( # To show 3400 line when bars are low
            type = "line",
            line = list(color = "#FFFFFF"),
            layer = "below",
            opacity = 0.0,
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = 3401,
            y1 = 3401,
            yref = "y"
          )
        ),
      xaxis = list(
        fixedrange = TRUE,
        linewidth = 0,
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Year"
        ),
        zeroline = FALSE
      ),
      yaxis = list(
        fixedrange = TRUE,
        gridcolor = "#c9c9c9",
        ticktext = list("0", "700", "1200", "1500", "1800", "2200", "2400", "2800", "3000", "3400"),
        tickvals = list(0, 700, 1200, 1500, 1800, 2200, 2400, 2800, 3000, 3400),
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Degree Days Fahrenheit"
        ),
        zeroline = TRUE,
        zerolinecolor = "#c9c9c9"
      ),
      yaxis2 = list(
        fixedrange = TRUE,
        gridcolor = "#c9c9c9",
        matches = "y",
        ticktext = list(
          "Planting", 
          "Pinhead Square", 
          "First Flower", 
          "One-inch Boll", 
          "Peak Bloom (Short)", 
          "Peak Bloom (Long)", 
          "Cutout (Short)", 
          "Cutout (Long)", 
          "Terminate (Short)", 
          "Terminate (Long)"
        ),
        tickvals = list(0, 700, 1200, 1500, 1800, 2200, 2400, 2800, 3000, 3400),
        title = list(
          font = list(size = 14),
          standoff = 25,
          text = "Cotton Growth Stage"
        ),
        overlaying = "y",
        side = "right",
        zeroline = TRUE,
        zerolinecolor = "#c9c9c9"
      )
    )
  
  return(figure)
}
