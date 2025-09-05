#' `fxn_navsetCardTabTitle.R` - Build title for container of navigation panels
#' 
#' @param azmetStation - AZMet station selection by user
#' @return `navsetCardTabTitle` - Title for container of navigation panels


fxn_navsetCardTabTitle <- function(azmetStation) {
  navsetCardTabTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("graph-up"), 
          htmltools::HTML("&nbsp;"),
          htmltools::HTML("&nbsp;"),
          toupper(
            paste0(
              "Cotton Heat Stress Data at the AZMet ", azmetStation, " Station"
            )
          ),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Click or tap and drag to zoom into an area of interest. Hover over data for variable values. Click or tap on legend items to toggle data visibility. Select from the icons to the right of the graph for additional functionality.",
            id = "infoFigureTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "navset-card-tab-title"
    )
  
  return(navsetCardTabTitle)
}
