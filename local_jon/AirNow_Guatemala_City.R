# Get data for Guatemala City

library(MazamaCoreUtils)
logger.setLevel(DEBUG)

library(AirMonitorIngest)
setAPIKey("airnow", "<YOUR_API_KEY>")

tblList <- list()

for ( year in 2010:2022 ) {

  starttime <- paste0(year, "0101")
  endtime <- paste0((year + 1), "0101")

  result <- try({

    tbl <-
      airnow_getDataCustom(
        parameterName = "PM2.5",
        starttime = starttime,
        endtime = endtime,
        timezone = "UTC",
        bbox = c(-91.4, 13.7, -89.4, 15.7)
      )

  }, silent = FALSE)

  if ( "try-error" %in% class(result) ) {
    next
  }

  tblList[[year]] <- tbl

}

tbl <-
  dplyr::bind_rows(tblList) %>%
  dplyr::arrange(.data$utcTime)

readr::write_csv(tbl, file = "Guatemala_City_PM2.5_2010-2022.csv")

# FROM Sites metadata:
# 320GT1010001|GT1010001|320GT1010001|PM2.5|Permanent|0001|Guatemala City|Active|GT1|U.S. Department of State Guatemala - Guatemala City|DSGT|14.989397|-90.522408||-6.00|GT||||||

plot(tbl$utcTime, tbl$parameterRawConcentration, pch = 15, col = adjustcolor("black", 0.2), xlab = "", ylab = "PM2.5 (ug/m3)", main = "Guatemala City PM2.5", las = 1)
AirMonitor::addAQILines()
AirMonitor::addAQIStackedBar()
points(tbl$utcTime, tbl$parameterRawConcentration, pch = 15, col = adjustcolor("black", 0.2))
