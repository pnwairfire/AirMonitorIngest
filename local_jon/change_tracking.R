# Change tracking example

# ===== DATA ===================================================================

library(MazamaCoreUtils)
logger.setLevel(TRACE)

library(AirMonitorIngest)

trackingFile <- path.expand("~/Data/monitoring-latency-v1/airnow/airnow_permanent_PM2.5_202110.rda")

if ( file.exists(trackingFile) ) {
  load(trackingFile)
}

now <- lubridate::now(tzone = "UTC")
#starttime <- now - lubridate::dhours(23)
starttime <- now - lubridate::dhours(0)

# Get the data

updateTbl <-

  # Get PM2.5 measurements, times and AQSIDs
  airnow_api_getData(
    starttime = starttime,
    endtime = now,
    pollutant = "PM2.5",
    monitorType = "permanent"
  ) %>%
  dplyr::select(utcTime, parameterConcentration, parameterRawConcentration, parameterAQI, AQSID) %>%
  dplyr::distinct() %>%

  # Add a data access time
  dplyr::mutate(
    dataAccessTime = now
  ) %>%
  dplyr::rename(
    measurementTime = utcTime
  ) %>%

  # Simplified dataset with just what we need
  dplyr::select(dataAccessTime, measurementTime, AQSID, parameterConcentration, parameterRawConcentration, parameterAQI)


if ( !exists("latencyTbl") ) {

  latencyTbl <- updateTbl

} else {

  # Create a new table with only distinct measurements
  latencyTbl <-
    dplyr::bind_rows(latencyTbl, updateTbl) %>%
    dplyr::distinct(measurementTime, AQSID, parameterConcentration, parameterRawConcentration, parameterAQI, .keep_all = TRUE)

}

save(latencyTbl, file = trackingFile)


# ===== PLOTTING ===============================================================


if ( FALSE ) {



  library(ggplot2)

  tbl <- get(load(trackingFile))

  # Find duplicate elements
  # See: https://github.com/sfirke/janitor/blob/main/R/get_dupes.R

  # Find AQSIDs with values that have been replaced
  interesting_AQSIDs <-
    tbl %>%
    dplyr::add_count(
      measurementTime, AQSID,
      name = "dupe_count"
    ) %>%
    dplyr::filter(dupe_count > 1) %>%
    dplyr::pull(AQSID) %>%
    sort() %>% unique()

  # Pick an AQSID
  if ( length(interesting_AQSIDs) == 0 ) {
    AQSID <- sample(tbl$AQSID, 1)
  } else {
    AQSID <- sample(interesting_AQSIDs, 1)
  }

  # Duplicate at 2021-10-22 18:00 -- "191471002"

  # Prepare plotting data
  plotData <-
    tbl %>%
    dplyr::filter(AQSID == !!AQSID) %>%
    dplyr::mutate(
      latency = as.numeric(difftime(dataAccessTime, measurementTime, unit = "mins"))
    ) %>%
    dplyr::arrange(desc(dataAccessTime))

  # Prepare last received version
  lastReceivedData <-
    plotData %>%
    dplyr::distinct(measurementTime, .keep_all = TRUE)


  # Simple plot
  plotData %>%
    # Plot begins here
    ggplot() +
    geom_point(
      aes(measurementTime, parameterConcentration),
      color = "red",
      size = 2
    ) +
    geom_line(
      data = lastReceivedData,
      mapping = aes(measurementTime, parameterConcentration)
    ) +
    geom_point(
      data = lastReceivedData,
      mapping = aes(measurementTime, parameterConcentration),
      color = "black",
      size = 2
    ) +
    ggtitle(AQSID)

  # Latency plot
  plotData %>%
    # Plot begins here
    ggplot() +
    geom_point(
      aes(measurementTime, latency),
      color = "red",
      size = 2
    ) +
    geom_point(
      data = lastReceivedData,
      mapping = aes(measurementTime, latency),
      color = "black",
      size = 2
    ) +
    ggtitle(AQSID)





}

