# Files --------------------

azmetStations <- 
  vroom::vroom(
    file = "aux-files/azmet-stations-api-db.csv", 
    delim = ",", 
    col_names = TRUE, 
    show_col_types = FALSE
  )


# Variables --------------------

azmetStations <- azmetStations |>
  dplyr::filter(stationName != "Test")

apiStartDate <- as.Date("2021-01-01")

if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-02-02"))) {
  initialEndDate <- 
    as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-11-15"))
} else {
  initialEndDate <- 
    (Sys.Date() - 1)
}

if (Sys.Date() < as.Date(paste0(lubridate::year(Sys.Date()), "-02-02"))) {
  initialPlantingDate <- 
    as.Date(paste0((lubridate::year(Sys.Date()) - 1), "-02-01"))
} else {
  initialPlantingDate <- 
    as.Date(paste0(lubridate::year(Sys.Date()), "-02-01"))
}

# Cotton growth stages, defined by 86/55 F heat units after planting, used in figure
dataCottonGrowthStages <- data.frame(
  huapValue = 
    c(
      0, 
      700, 
      1200, 
      1500, 
      1800, 
      2200, 
      2400, 
      2800, 
      3000, 
      3400
    ), 
  growthStage = 
    c(
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
    )
)
