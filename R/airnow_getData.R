#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Download and parse hourly data from the AirNow data API
#'
#' @param starttime Desired start datetime (ISO 8601).
#' @param endtime Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param parameterName One or more EPA AQS criteria parameter names.
#' @param monitorType Subset of all monitors to select.
#' @param baseUrl Base URL for archived hourly data.
#'
#' @return Tibble of AirNow hourly data.
#'
#' @description This function uses the AirNow data webservice to retrieve
#' subsets of data that do not exceed a maximum data size which causes
#' errors.
#'
#' Datetimes can be anything that is understood by
#' \code{MazamaCoreUtils::parseDatetime()}. For non-POSIXct values, the
#' recommended format is "YYYY-mm-dd HH:00:00" or just "YYYYmmddhh".
#'
#'
#' @examples
#' \dontrun{
#' library(AirMonitorIngest)
#'
#' tbl <-
#'   airnow_getData(
#'     starttime = 2021101200,
#'     endtime = 2021101300,
#'     timezone = "America/Los_Angeles",
#'     parameterName = "PM2.5",
#'     monitorType = "permanent"
#'    )
#'
#' }

airnow_getData <- function(
  starttime = NULL,
  endtime = NULL,
  timezone = "UTC",
  parameterName = c("PM2.5"), ###, "CO", "OZONE", "PM10"),
  monitorType = c("both", "permanent", "mobile"),
  baseUrl = "https://www.airnowapi.org/aq/data/"
) {

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- airnow_getData() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(starttime)
  MazamaCoreUtils::stopIfNull(endtime)
  MazamaCoreUtils::stopIfNull(timezone)

  parameterName <- match.arg(parameterName, several.ok = TRUE)
  monitorType <- match.arg(monitorType, several.ok = FALSE)

  # TODO:  Only one parameter at a time for now
  if ( length(parameterName) > 1 )
    stop("only a single 'parameterName' may be specified")

  MazamaCoreUtils::setIfNull(baseUrl, "https://www.airnowapi.org/aq/data/")

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  starttime <- MazamaCoreUtils::parseDatetime(starttime, timezone = timezone)
  endtime <- MazamaCoreUtils::parseDatetime(endtime, timezone = timezone)

  airnow_API_KEY <- getAPIKey("airnow")
  if ( is.null(airnow_API_KEY) )
    stop(paste0(
      "The AirNow API_KEY is not set. Please set it with:\n\n",
      "  setAPIKey(\"airnow\", <your-api-key>)"
    ))

  # ----- Loop over multi-hour chunks ------------------------------------------

  parameterNameCount <- length(parameterName)

  if ( parameterNameCount == 1 ) {
    hourCount <- 12
  } else if ( parameterNameCount == 2 ) {
    hourCount <- 6
  } else if ( parameterNameCount == 3 ) {
    hourCount <- 4
  } else if ( parameterNameCount == 4 ) {
    hourCount <- 3
  } else if ( parameterNameCount <= 6 ) {
    hourCount <- 2
  } else {
    hourCount <- 1
  }

  byString <- sprintf("%d hours", hourCount)

  startTimes <- seq(starttime, endtime, by = byString)
  endTimes <- c((startTimes - 3600)[-1], endtime)

  # Pre-allocate an empty list of the appropriate length
  tblList <- vector(mode = "list", length = length(startTimes))

  for ( i in seq_along(startTimes) ) {

    if ( MazamaCoreUtils::logger.isInitialized() ) {

      chunkHourCount <-
        difftime(endTimes[i], startTimes[i], units = "hours") %>%
        as.numeric() + 1

      logger.trace(
        "Requesting %d hours of data starting at %s ",
        chunkHourCount,
        strftime(startTimes[i], tz = "UTC", usetz = TRUE)
      )

    }

    # Obtain a chunk of AirNow data
    result <- try({
      tbl <-
        airnow_getDataSubset(
          starttime = startTimes[i],
          endtime = endTimes[i],
          timezone = timezone,
          parameterName = parameterName,
          monitorType = monitorType,
          baseUrl = baseUrl
        )
    }, silent = TRUE)

    if ( "try-error" %in% class(result) ) {
      err_msg <- stringr::str_trim(geterrmessage())
      logger.warn("Unable to download data: %s",err_msg)
      next
    }

    tblList[[i]] <- tbl

  }

  # Combine all tibbles, removing duplicates
  tbl <-
    dplyr::bind_rows(tblList) %>%
    dplyr::distinct()

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Downloaded and parsed %d rows of AirNow data", nrow(tbl))

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  library(AirMonitorIngest)

  starttime <- 2021101802
  endtime <- 2021101802
  timezone <- "America/Los_Angeles"
  parameterName <- "PM2.5"
  monitorType <- "mobile"
  monitorType <- "permanent"



  tbl <-
    airnow_getData(
      starttime = starttime,
      endtime = endtime,
      timezone = timezone,
      parameterName = parameterName,
      monitorType = monitorType
    )




  MazamaLocationUtils::table_leaflet(
    tbl,
    maptype = "terrain",
    extraVars = c("siteName", "agencyName", "AQSID"),
    weight = 1
  )


}
