#' @export
#'
#' @title Download and parse AirNow sites metadata
#'
#' @param url URL of the AirNow monitoring site locations file.
#' @param quiet Logical passed on to
#' \code{readr::read_delim(progress = !quiet)}.
#'
#' @description
#' The \url{https://airnowtech.org} site provides both air pollution
#' monitoring data as well as monitoring site location metadata. This function
#' retrieves the most recent version of the site location metadata file and
#' returns it as a tibble.
#'
#' A description of the data format is publicly available at the
#' \href{https://docs.airnowapi.org/docs/MonitoringSiteV2FactSheet.pdf}{Monitoring Site FV2 act Sheet}.
#'
#' @return Tibble of AirNow site metadata.
#'
#' @examples
#' \dontrun{
#' library(AirMonitorIngest)
#'
#' # Create a directory specifically for AirNow data
#' dir.create("~/Data/AirNow", recursive = TRUE)
#'
#' # Set logging level so messages and errors will appear in the console
#' MazamaCoreUtils::initializeLogging(logDir = "~/Data/AirNow")
#' logger.setLevel(TRACE)
#'
#' # Download and parse site metadata
#' airnow_sites <- airnow_getSites()
#' }

airnow_getSites <- function(
  url = "https://files.airnowtech.org/airnow/today/Monitoring_Site_Locations_V2.dat",
  quiet = TRUE
) {

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- airnow_getSites() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  url <- MazamaCoreUtils::setIfNull(url, "https://files.airnowtech.org/airnow/today/Monitoring_Site_Locations_V2.dat")
  quiet <- MazamaCoreUtils::setIfNull(quiet, TRUE)

  # ----- Download data --------------------------------------------------------

  # Convert to inital lower case for internal consistency
  col_names <- c(
    "stationID", "AQSID", "fullAQSID", "parameterName", "monitorType",
    "siteCode", "siteName", "status", "agencyID", "agencyName", "EPARegion",
    "latitude", "longitude", "elevation", "GMTOffsetHours",
    "countryFIPS", "CBSA_ID", "CBSA_Name", "stateAQSCode", "stateAbbreviation",
    "countyAQSCode", "countyName"
  )

  col_types <- paste0("ccccc", "cccccc", "dddd", "ccccc", "cc")

  locale <- readr::locale(encoding = "UTF-8")

  # Read in text as a dataframe
  tbl <- readr::read_delim(
    url,
    delim = "|",
    col_names = col_names,
    col_types = col_types,
    locale = locale,
    skip = 1,
    progress = !quiet,
    show_col_types = !quiet
  )

  return(tbl)

}

