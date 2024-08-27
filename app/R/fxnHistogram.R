#' `fxnHistogram` generates histogram of cotton heat stress categories
#' 
#' @param inData - data table of seasonal cotton heat stress values by year
#' @return: `histogram` - png of histogram


fxnHistogram <- function(inData) {
  
  # Adjust day-of-year values for leap years to correctly line up month and day values on plot
  for (yr in unique(inData$date_year)) {
    if (lubridate::leap_year(yr) == TRUE) {
      inData$date_doy[which(inData$date_year == yr)] <- 
        inData$date_doy[which(inData$date_year == yr)] - 1
    }
  }
  
  inData <- inData %>%
    dplyr::filter(is.na(heatstress_cotton_meanF) == FALSE)
  
  if (max(inData$date_year) - min(inData$date_year) == 1) {
    previousYearsLabel <- min(inData$date_year)
  } else {
    previousYearsLabel <- 
      paste(min(inData$date_year), max(inData$date_year) - 1, sep = "-")
  }
  
  xAxisBreaks <- c(121, 152, 182, 213, 244, 274)
  xAxisLabels <- c("May", "Jun", "Jul", "Aug", "Sep", "Oct")
  
  histogram <- ggplot2::ggplot(data = inData) +
    
    # https://www.color-hex.com/color-palette/1041718  
    
    # All growing seasons
    geom_histogram(
      mapping = aes(x = date_doy), 
      binwidth = 1, center = 0, color = "#FFFFFF", fill = "#989898", linewidth = 0.1
    ) +
    
    geom_label(
      data = dplyr::filter(inData, datetime == max(datetime)),
      mapping = aes(
        x = date_doy + 3.0, 
        y = 1.75, 
        label = previousYearsLabel
      ),
      alpha = 1.0, color = "#989898", fill = "#FFFFFF", fontface = "bold", hjust = 0.0, size = 4
    ) +
    
    # Current growing season, as a visual overlay
    geom_histogram(
      data = dplyr::filter(inData, date_year == max(inData$date_year)), 
      mapping = aes(x = date_doy), 
      binwidth = 1, center = 0, color = "#FFFFFF", fill = "#3b3b3b", linewidth = 0.1
    ) +
    
    geom_label(
      data = dplyr::filter(inData, datetime == max(datetime)),
      mapping = aes(x = date_doy + 3.0, y = 0.5, label = date_year),
      alpha = 1.0, color = "#3b3b3b", fill = "#FFFFFF", fontface = "bold", hjust = 0.0, size = 4
    ) +
    
    # Month-day marker
    geom_vline(
      xintercept = dplyr::filter(inData, datetime == max(datetime))$date_doy,
      color = "#989898", linewidth = 0.6, linetype = "dotted", lineend = "round"
    ) +
  
    facet_wrap(
      vars(heatstress_categories), ncol = 1, scales = "fixed", strip.position = "top"
    ) +
    
    labs(x = "\nMonth\n", y = "Number of Times in Level\n") +
    
    scale_x_continuous(
      breaks = xAxisBreaks, 
      labels = xAxisLabels,
      expand = expansion(mult = c(0.00, 0.00))
    ) +
    
    scale_y_continuous(
      breaks = seq(from = 0, to = 100, by = 1), 
      labels = seq(from = 0, to = 100, by = 1),
      limits = c(0, length(unique(inData$date_year))),
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
      axis.title.x = element_text(color = "#989898", face = "plain", size = 9, hjust = 0.0),
      #axis.title.x.top,
      #axis.title.x.bottom,
      axis.title.y = element_text(color = "#989898", face = "plain", size = 9, hjust = 0.0),
      #axis.title.y.left,
      #axis.title.y.right,
      #axis.text,
      axis.text.x = element_text(color = "#989898", face = "plain", size = 9),
      #axis.text.x.top,
      #axis.text.x.bottom,
      axis.text.y = element_text(color = "#989898", face = "plain", size = 9),
      #axis.text.y.left,
      #axis.text.y.right,
      #axis.ticks,
      #axis.ticks.x,
      #axis.ticks.x.top,
      axis.ticks.x.bottom = 
        element_line(
          color = "#c9c9c9", linewidth = 0.25, linetype = "solid", lineend = "round", 
          arrow = NULL, inherit.blank = FALSE
        ),
      #axis.ticks.y,
      axis.ticks.y.left = 
        element_line(
          color = "#c9c9c9", linewidth = 0.25, linetype = "solid", lineend = "round", 
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
      #axis.line.y,
      axis.line.y.left = element_blank(),
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
          color = "#c9c9c9", linewidth = 0.25, linetype = "solid", lineend = "round", 
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
      strip.text.x = element_text(color = "#989898", face = "plain", hjust = 0.0, size = 10)
      #strip.text.y,
      #strip.switch.pad.grid,
      #strip.switch.pad.wrap,
      #...,
      #complete = FALSE,
      #validate = TRUE
    )
  
  return(histogram)
}
