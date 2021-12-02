# AIRSIS test

library(MazamaCoreUtils)
library(AirMonitorIngest)

distanceThreshold <- 1000

# ----- BAM.1020 ---------------------------------------------------------------

airsis_data <-

  airsis_downloadData(
    startdate = MazamaCoreUtils::parseDatetime("2013-05-20", timezone = "UTC"),
    enddate = MazamaCoreUtils::parseDatetime("2013-05-30", timezone = "UTC"),
    timezone = "UTC",
    provider = "APCD",
    unitID = "1012"
  ) %>%

  airsis_parseData(
    codeDir = "airsis_codeDir"
  ) %>%

  airsis_QC_BAM.1020(
    flagAndKeep = FALSE
  ) %>%

  addClustering(
    clusterDiameter = distanceThreshold,
    lonVar = "longitude",
    latVar = "latitude",
    maxClusters = 50,
    flagAndKeep = FALSE
  )
