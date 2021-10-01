# First tests

library(dplyr)

library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(MazamaLocationUtils)
library(AirMonitorIngest)

MazamaLocationUtils::mazama_initialize("~/Data/Spatial")
MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")

# Get appropriate data

parameterCode <- "88101"
sites_locationTbl <- MazamaLocationUtils::table_load("AQS_88101_sites")
AQS_monitors <- epa_aqs_getMonitors(downloadDir = "~/Data/EPA")
AQS_data <- epa_aqs_parseHourlyData("~/Data/EPA/hourly_88101_2008.zip")

# Create meta

meta <- epa_aqs_createMeta(sites_locationTbl, AQS_monitors, parameterCode)

data <- epa_aqs_createData(AQS_data, meta, parameterCode)

# Modify meta to match 'ws_monitor' sensibilities

deviceDeploymentIDs <- names(data)[2:ncol(data)]

meta <-
  meta %>%
  dplyr::filter(deviceDeploymentID %in% deviceDeploymentIDs)

meta$monitorID <- meta$deviceDeploymentID

# Create the 'ws_monitor' data list
ws_monitor <- list(meta = meta, data = data)
ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

AirMonitorPlots::monitor_ggTimeseries(ws_monitor)

