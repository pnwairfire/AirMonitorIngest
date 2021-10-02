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

# ----- Save as .rda, .csv and .csv.gz -----------------------------------------

deviceDeploymentIDs <- names(data_ORIG)[2:ncol(data_ORIG)]

meta <-
  meta_ORIG %>%
  dplyr::filter(deviceDeploymentID %in% deviceDeploymentIDs)

preferredColumns <- c('datetime', meta$deviceDeploymentID)

data <-
  data_ORIG %>%
  dplyr::select(dplyr::all_of(preferredColumns))

# Write 'meta'

save(
  meta,
  file = "~/Data/monitoring/EPA/88101/2008/epa_88101_2008_meta.rda"
)

readr::write_csv(
  meta,
  file = "~/Data/monitoring/EPA/88101/2008/epa_88101_2008_meta.csv",
  na = "",
  quote = "all"
)

readr::write_csv(
  meta,
  file = "~/Data/monitoring/EPA/88101/2008/epa_88101_2008_meta.csv.gz",
  na = "",
  quote = "all"
)

# Write 'data'

save(
  data,
  file = "~/Data/monitoring/EPA/88101/2008/epa_88101_2008_data.rda"
)

readr::write_csv(
  data,
  file = "~/Data/monitoring/EPA/88101/2008/epa_88101_2008_data.csv",
  na = "",
  quote = "all"
)

readr::write_csv(
  data,
  file = "~/Data/monitoring/EPA/88101/2008/epa_88101_2008_data.csv.gz",
  na = "",
  quote = "all"
)


