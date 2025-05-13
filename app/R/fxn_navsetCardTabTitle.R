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
          )
        ),
      ),
      
      class = "navset-card-tab-title"
    )
  
  return(navsetCardTabTitle)
}
