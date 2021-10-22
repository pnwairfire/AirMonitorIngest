# Change tracking example

# ===== DATA ===================================================================

library(MazamaCoreUtils)
logger.setLevel(TRACE)

library(AirMonitorIngest)

trackingFile <- path.expand("~/Data/AirNow/airnow_permanent_pm25_tracking.rda")

if ( file.exists(trackingFile) ) {
  load(trackingFile)
}

now <- lubridate::now(tzone = "UTC")
starttime <- now - lubridate::dhours(11)

# Get the data

updateTbl <-

  # Get PM2.5 measurements, times and AQSIDs
  airnow_api_getData(
    starttime = starttime,
    endtime = now,
    pollutant = "PM2.5",
    monitorType = "permanent",
    includeSiteMeta = TRUE
  ) %>%
  dplyr::select(utcTime, parameterValue, AQSID) %>%
  dplyr::distinct() %>%

  # Add a data access time
  dplyr::mutate(
    dataAccessTime = now
  ) %>%
  dplyr::rename(
    measurementTime = utcTime
  ) %>%

  # Simplified dataset with just what we need
  dplyr::select(dataAccessTime, measurementTime, AQSID, parameterValue)


if ( !exists("trackingTbl") ) {

  trackingTbl <- updateTbl

} else {

  # Create a new table with only distinct measurements
  trackingTbl <-
    dplyr::bind_rows(trackingTbl, updateTbl) %>%
    dplyr::distinct(measurementTime, AQSID, parameterValue, .keep_all = TRUE)

}

save(trackingTbl, file = trackingFile)


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
      aes(measurementTime, parameterValue),
      color = "red",
      size = 2
    ) +
    geom_line(
      data = lastReceivedData,
      mapping = aes(measurementTime, parameterValue)
    ) +
    geom_point(
      data = lastReceivedData,
      mapping = aes(measurementTime, parameterValue),
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

