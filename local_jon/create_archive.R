# First tests

library(MazamaCoreUtils)
library(AirMonitorIngest)

MazamaLocationUtils::mazama_initialize("~/Data/Spatial")
MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")

parameterCodes <- c("42101", "44201", "81102", "88101", "88502")

for ( parameterCode in parameterCodes ) {

  logDir <- file.path("~/Data/monitoring/epa_aqs/", parameterCode)
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  MazamaCoreUtils::initializeLogging(logDir)
  MazamaCoreUtils::logger.setLevel(DEBUG)

  logger.debug("===== parameterCode %s =====", parameterCode)

  library(MazamaLocationUtils)
  MazamaLocationUtils::mazama_initialize()
  MazamaLocationUtils::setLocationDataDir("~/Data/monitoring/known_locations")

  sites_locationTbl <-
    MazamaLocationUtils::table_load(sprintf("AQS_%s_sites", parameterCode))

  # 42101 starts in 1980
  # 44201 starts in 1980
  # 81102 starts in 1988
  # 88101 starts in 2008
  # 88502 starts in 1998

  epa_aqs_createLocalArchive(
    sites_locationTbl = sites_locationTbl,
    downloadDir = "~/Data/EPA",
    parameterCode = parameterCode,
    years = 2015:2018,
    archiveBaseDir = "~/Data/monitoring",
    quiet = FALSE
  )

}
