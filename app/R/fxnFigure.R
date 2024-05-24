#' `fxnFigure` generates line graph of estimated canopy temperature values from current/recent years with cotton heat stress categories
#' 
#' @param inData - data table of seasonal cotton heat stress values by year
#' @param azmetStation - AZMet station selection by user
#' @return: `figure` - png of figure


fxnFigure <- function(inData, azmetStation) {
  
  # Adjust day-of-year values for leap years to correctly line up month and day values on plot
  for (yr in unique(inData$date_year)) {
    if (lubridate::leap_year(yr) == TRUE) {
      inData$date_doy[which(inData$date_year == yr)] <- 
        inData$date_doy[which(inData$date_year == yr)] - 1
    }
  }
  
  xAxisBreaks <- c(121, 152, 182, 213, 244, 274)
  xAxisLabels <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct")
  
  figure <- ggplot2::ggplot(data = inData) +
    
    # Heat stress zones: shading -----
    
    # Level 1
    annotate(
      geom = "rect", 
      xmin = min(inData$date_doy), 
      xmax = Inf, 
      ymin = 82.4, 
      ymax = 86.0, 
      #fill = "#9EABAE", 
      fill = "#e0e0e0", 
      alpha = 0.5
    ) +
    
    # Level 2
    annotate(
      geom = "rect", 
      xmin = min(inData$date_doy), 
      xmax = Inf, 
      ymin = 86.0, 
      ymax = Inf, 
      #fill = "#9EABAE", 
      fill = "#bdbdbd",
      alpha = 0.5
    ) +
    
    # Heat stress time series -----
    
    # Previous years
    geom_line(
      data = dplyr::filter(inData, date_year <= (max(date_year) - 1)), 
      mapping = aes(x = date_doy, y = heatstress_cotton_meanF, group = date_year), 
      color = "#bdbdbd", lineend = "round", linejoin = "round", linewidth = 0.8, alpha = 1.0
    ) +
  
  
    #geom_line(
    #  data = dplyr::filter(inData, date_year == (max(date_year) - 1)), 
    #  mapping = aes(x = date_doy, y = heatstress_cotton_meanF), 
    #  color = "#378DBD", linewidth = 0.5, alpha = 1.0
    #) +
    
    #geom_point(data = dplyr::filter(dplyr::filter(inData, date_year == min(date_year)), datetime == max(datetime)), mapping = aes(x = date_doy, y = heatstress_cotton_meanF), shape = 21, color = "#FFFFFF", fill = "#378DBD", size = 2, stroke = 0.5) +
    
    #annotate(geom = "text", label = min(inData$date_year), x = (dplyr::filter(dplyr::filter(inData, date_year == min(date_year)), datetime == max(datetime))$date_doy + 2.0), y = dplyr::filter(dplyr::filter(inData, date_year == min(date_year)), datetime == max(datetime))$heatstress_cotton_meanF, color = "#378DBD", size = 3, fontface = "plain", hjust = 0.0) +
    
    # Current year
    geom_line(
      data = dplyr::filter(inData, date_year == max(date_year)),
      mapping = aes(x = date_doy, y = heatstress_cotton_meanF), 
      color = "#343a40", linewidth = 1.0
    ) +
    
    geom_point(
      data = dplyr::filter(inData, datetime == max(datetime)), 
      mapping = aes(x = date_doy, y = heatstress_cotton_meanF), 
      color = "#FFFFFF", fill = "#343a40", shape = 21, size = 4, stroke = 0.5
    ) +
    
    annotate(
      geom = "text", 
      label = max(inData$date_year), 
      x = dplyr::filter(dplyr::filter(inData, date_year == max(date_year)), datetime == max(datetime))$date_doy + 3.0, 
      y = dplyr::filter(dplyr::filter(inData, date_year == max(date_year)), datetime == max(datetime))$heatstress_cotton_meanF, 
      color = "#343a40", fontface = "bold", hjust = 0.0, size = 4
    ) +
    
    # Heat stress zones: labels -----
  
    annotate(
      geom = "text", 
      label = "NO HEAT STRESS", 
      x = (min(inData$date_doy) + 2), 
      y = (78.8 + 0.5), 
      color = "#757575", 
      size = 3, 
      fontface = "bold", 
      hjust = 0.0, 
      vjust = 0.0
    ) +
    
    annotate(
      geom = "text", 
      label = "LEVEL 1 HEAT STRESS", 
      x = (min(inData$date_doy) + 2), 
      y = (82.4 + 0.5), 
      color = "#757575", 
      size = 3, 
      fontface = "bold", 
      hjust = 0.0, 
      vjust = 0.0
    ) +
    
    annotate(
      geom = "text", 
      label = "LEVEL 2 HEAT STRESS", 
      x = (min(inData$date_doy) + 2), 
      y = (86.0 + 0.5), 
      color = "#757575", 
      size = 3, 
      fontface = "bold", 
      hjust = 0.0, 
      vjust = 0.0
    ) +
    
    # Graph appearance -----
    
    labs(x = "\nMonth", y = "°F  ") +
    
    scale_x_continuous(
      breaks = xAxisBreaks, 
      labels = xAxisLabels,
      expand = expansion(mult = c(0.00, 0.08))
    ) +
    
    scale_y_continuous(
      breaks = seq(from = 0, to = 150, by = 10), 
      labels = seq(from = 0, to = 150, by = 10),
      #limits = c(min(inData$heatstress_cotton_meanF), max(inData$heatstress_cotton_meanF)),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    
    theme_minimal() +
    
    theme( # https://ggplot2.tidyverse.org/reference/theme.html
      #line,
      #rect,
      text = element_text(family = "sans"),
      #title,
      #aspect.ratio,
      #axis.title,
      axis.title.x = element_text(color = "#757575", face = "plain", size = 9, hjust = 0.0),
      #axis.title.x.top,
      #axis.title.x.bottom,
      axis.title.y = element_text(color = "#757575", face = "plain", size = 9, angle = 0, vjust = 0.0),
      #axis.title.y.left,
      #axis.title.y.right,
      #axis.text,
      axis.text.x = element_text(color = "#757575", face = "plain", size = 9),
      #axis.text.x.top,
      #axis.text.x.bottom,
      axis.text.y = element_text(color = "#757575", face = "plain", size = 9),
      #axis.text.y.left,
      #axis.text.y.right,
      #axis.ticks,
      #axis.ticks.x,
      #axis.ticks.x.top,
      axis.ticks.x.bottom = 
        element_line(
          color = "#e0e0e0", linewidth = 0.25, linetype = "solid", lineend = "round", 
          arrow = NULL, inherit.blank = FALSE
        ),
      #axis.ticks.y,
      axis.ticks.y.left = 
        element_line(
          color = "#e0e0e0", linewidth = 0.25, linetype = "solid", lineend = "round", 
          arrow = NULL, inherit.blank = FALSE
        ),
      #axis.ticks.y.right,
      #axis.ticks.length,
      #axis.ticks.length.x,
      #axis.ticks.length.x.top,
      #axis.ticks.length.x.bottom,
      #axis.ticks.length.y,
      #axis.ticks.length.y.left,
      #axis.ticks.length.y.right,
      #axis.line,
      #axis.line.x,
      #axis.line.x.top,
      axis.line.x.bottom = element_blank(),
        #element_line(
        #  color = "#343a40", linewidth = 0.25, linetype = "solid", lineend = "round", arrow = NULL, inherit.blank = FALSE
        #),
      #axis.line.y,
      axis.line.y.left = element_blank(),
        #element_line(
        #  color = "#343a40", linewidth = 0.25, linetype = "solid", lineend = "round", arrow = NULL, inherit.blank = FALSE
        #),
      #axis.line.y.right,
      #legend.background,
      #legend.margin,
      #legend.spacing,
      #legend.spacing.x,
      #legend.spacing.y,
      #legend.key,
      #legend.key.size,
      #legend.key.height,
      #legend.key.width,
      #legend.text,
      #legend.text.align,
      #legend.title,
      #legend.title.align,
      #legend.position,
      #legend.direction,
      #legend.justification,
      #legend.box,
      #legend.box.just,
      #legend.box.margin,
      #legend.box.background,
      #legend.box.spacing,
      #panel.background,
      #panel.border,
      #panel.spacing,
      #panel.spacing.x,
      #panel.spacing.y,
      #panel.grid,
      panel.grid.major =
        element_line(
          color = "#e0e0e0", linewidth = 0.25, linetype = "solid", lineend = "round", 
          arrow = NULL, inherit.blank = FALSE
        ),
      panel.grid.minor = element_blank(),
      #panel.grid.major.x,
      #panel.grid.major.y,
      #panel.grid.minor.x,
      #panel.grid.minor.y,
      #panel.ontop,
      #plot.background,
      #plot.title,
      #plot.title.position,
      #plot.subtitle,
      #plot.caption,
      #plot.caption.position,
      #plot.tag,
      #plot.tag.position,
      #plot.margin,
      #strip.background,
      #strip.background.x,
      #strip.background.y,
      #strip.clip,
      #strip.placement,
      #strip.text,
      #strip.text.x,
      #strip.text.y,
      #strip.switch.pad.grid,
      #strip.switch.pad.wrap,
      #...,
      #complete = FALSE,
      #validate = TRUE
    )
  
  return(figure)
}
