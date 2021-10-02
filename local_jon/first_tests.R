# First tests

library(dplyr)

library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(MazamaLocationUtils)
library(AirMonitorIngest)

MazamaLocationUtils::mazama_initialize("~/Data/Spatial")
MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")

# ----- Get meta and data ------------------------------------------------------

parameterCode <- "88101"
sites_locationTbl <- MazamaLocationUtils::table_load("AQS_88101_sites")
AQS_monitors <- epa_aqs_getMonitors(downloadDir = "~/Data/EPA")
AQS_data <- epa_aqs_parseHourlyData("~/Data/EPA/hourly_88101_2008.zip")

meta_ORIG <- epa_aqs_createMeta(sites_locationTbl, AQS_monitors, parameterCode)

data_ORIG <- epa_aqs_createData(AQS_data, meta_ORIG, parameterCode)

# ----- Minimal 'air_monitor' object -------------------------------------------

# Modify meta to match prototype 'air_monitor' sensibilities

deviceDeploymentIDs <- names(data)[2:ncol(data_ORIG)]

meta <-
  meta_ORIG %>%
  dplyr::filter(deviceDeploymentID %in% deviceDeploymentIDs)

meta$monitorID <- meta$deviceDeploymentID

# Create the 'ws_monitor' data list
ws_monitor <- list(meta = meta, data = data_ORIG)
ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

AirMonitorPlots::monitor_ggTimeseries(ws_monitor)

# ----- PWFSLSmoke viable version ----------------------------------------------

meta <-
  meta_ORIG %>%
  dplyr::filter(deviceDeploymentID %in% deviceDeploymentIDs) %>%
  dplyr::mutate(
    siteName = locationName,
    monitorID = sprintf("%s_%02d", AQS_AQSID, as.numeric(AQS_POC))
  )

data <- data_ORIG

monitorColumnNames <- names(data)[2:ncol(data)]
parts <- stringr::str_split_fixed(monitorColumnNames, "_", 3)
newColumnNames <- sprintf("%s_%s", parts[,2], parts[,3])

names(data) <- c('datetime', newColumnNames)

preferredColumns <- c('datetime', meta$monitorID)

data <-
  data %>%
  dplyr::select(dplyr::all_of(preferredColumns))

# Remove 'tibble-ness'
class(meta) <- "data.frame"
class(data) <- "data.frame"
rownames(meta) <- meta$monitorID

# Create the 'ws_monitor' data list
ws_monitor <- list(meta = meta, data = data)
ws_monitor <- structure(ws_monitor, class = c("ws_monitor", "list"))

library(PWFSLSmoke)

ws_monitor %>%
  monitor_timeseriesPlot(pch = 15, cex = 0.5, col = adjustcolor('black', 0.1))

ws_monitor %>%
  monitor_subset(stateCodes = "HI", tlim = c(20080901, 20081001)) %>%
  monitor_leaflet()

ws_monitor %>%
  monitor_subset(stateCodes = "HI", tlim = c(20080901, 20081001)) %>%
  monitor_subset(monitorIDs = "150012017_01") %>%
  monitor_dailyBarplot()

ws_monitor %>%
  monitor_subset(stateCodes = "HI", tlim = c(20080901, 20081001)) %>%
  ##monitor_subset(monitorIDs = "150012017_01") %>%
  AirMonitorPlots::monitor_ggDailyHourlyBarplot(columns = 2)






