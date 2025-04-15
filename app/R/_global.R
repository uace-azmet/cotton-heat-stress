# Libraries --------------------

library(azmetr)
library(dplyr)
library(english)
library(ggplot2)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)


# Files --------------------

# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)


# Variables --------------------

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

# Load auxilliary files
azmetStations <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)
