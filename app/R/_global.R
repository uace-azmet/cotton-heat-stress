# Libraries --------------------

library(azmetr)
library(bslib)
library(dplyr)
library(english)
library(htmltools)
library(lubridate)
library(reshape2)
library(shiny)
#library(shinyjs)
library(vroom)


# Files --------------------

# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)

azmetStations <- 
  vroom::vroom(
    file = "aux-files/azmet-stations-api-db.csv", 
    delim = ",", 
    col_names = TRUE, 
    show_col_types = FALSE
  )


# Variables --------------------

apiStartDate <- as.Date("2021-01-01")

azmetStations <- azmetStations |>
  dplyr::mutate(
    stationEndDate = dplyr::if_else(
      stationStatus == "active",
      lubridate::today(tzone = "America/Phoenix") - 1,
      stationEndDate
    )
  ) |>
  dplyr::mutate(stationStartDate = stationStartDate) |>
  dplyr::mutate(stationEndDate = stationEndDate) |>
  dplyr::filter(!stationName %in% c("Chino Valley", "Mohave ETo", "Test", "Wellton ETo", "Yuma Valley ETo"))

selectedTab <- shiny::reactiveVal(value = "estimatedCanopyTemperatures")
