# AIRSIS test

library(MazamaCoreUtils)
logger.setLevel(DEBUG)

library(AirMonitorIngest)

# Load R functions
R_files <- list.files("airsis_codeDir", pattern = ".+\\.R", full.names = TRUE)

for (file in R_files) {
  source(file.path(getwd(), file))
}

distanceThreshold <- 1000

# ----- BAM.1020 ---------------------------------------------------------------

provider <- "APCD"
unitID <- "1012"

airsis_data <-

  airsis_downloadData(
    startdate = MazamaCoreUtils::parseDatetime("2013-05-20", timezone = "UTC"),
    enddate = MazamaCoreUtils::parseDatetime("2013-05-30", timezone = "UTC"),
    timezone = "UTC",
    provider = "APCD",
    unitID = "1012"
  ) %>%

  airsis_parseAndQCData() %>%

  addClustering(
    clusterDiameter = distanceThreshold,
    lonVar = "longitude",
    latVar = "latitude",
    maxClusters = 50,
    flagAndKeep = FALSE
  ) %>%

  # Add device metadata
  dplyr::mutate(
    airsis_provider = tolower(!!provider),
    airsis_unitID = !!unitID
  )

