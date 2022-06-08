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
#' \href{https://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf}{Monitoring Site Fact Sheet}.
#'
#' @note As of October, 2021, the description above is incorrect. See the source
#' code for this function for a correct set of field names.
#'
#' @note As of October, 2021, the \code{monitoring_site_locations.dat} file has
#' an encoding of "CP437" (aka "Non-ISO extended-ASCII" or "IBMPC 437") and will
#' be converted to "UTF-8" so that French and Spanish language place names are
#' properly encoded in the returned dataframe.
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
#' airnow_sites <- airnow_getSites_v1()
#' }

airnow_getSites_v1 <- function(
  url = "https://files.airnowtech.org/airnow/today/monitoring_site_locations.dat",
  quiet = TRUE
) {

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- airnow_getSites() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  url <- MazamaCoreUtils::setIfNull(url, "https://files.airnowtech.org/airnow/today/monitoring_site_locations.dat")
  quiet <- MazamaCoreUtils::setIfNull(quiet, TRUE)

  # ----- Download data --------------------------------------------------------

  # NOTE:  Information on the structure of this file come from the Monitoring
  # NOTE:  Site Factsheet (June, 2019).
  # NOTE:    https://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf
  # NOTE:
  # NOTE:  THIS INFORMATION IS INCORRECT!
  # NOTE:
  # NOTE:  The column names below are correct.

  # AQSID|parameter name|site code|site name|status|
  # agency id|agency name|EPA region|latitude|longitude|
  # elevation|GMT offset|country code|MSA code|MSA name|
  # state code|state name|county code|county name
  #
  # 060410001|O3|0001|San Rafael|Active|
  # CA2|San Francisco Bay Area AQMD|R9|37.972200|-122.518900|
  # 0.900|- 8.00|US|||
  # 41860| San Francisco-Oakland-Fremont, CA |06|CA|06041|
  # MARIN||

  col_names <- c(
    "AQSID", "parameterName", "siteCode", "siteName", "status",
    "agencyID", "agencyName", "EPARegion", "latitude", "longitude",
    "elevation", "GMTOffsetHours", "countryCode", "empty1", "empty2",
    "FIPSMSACode", "MSAName", "FIPSStateCode", "stateCode", "GNISCountyCode",
    "countyName", "empty3", "empty4"
    )

  col_types <- paste0("ccccc", "cccdd", "ddccc", "ccccc", "ccc")

  # NOTE:  This file is not properly encoded.
  # NOTE:  Using the default encoding, we see this:

  # > dplyr::filter(tbl, AQSID == "800150581", parameterName == "PM2.5") %>%
  #   + dplyr::pull(siteName)
  # [1] "Nezahualc\xa2yotl"

  # NOTE:  Using encoding = "CP437" we see this:
  # > dplyr::filter(tbl, AQSID == "800150581", parameterName == "PM2.5") %>%
  #   + dplyr::pull(siteName)
  # [1] "Nezahualcóyotl"

  # Fix the encoding
  locale <- readr::locale(encoding = "CP437")

  # Read in text as a dataframe
  tbl <- readr::read_delim(
    url,
    delim = "|",
    col_names = col_names,
    col_types = col_types,
    locale = locale,
    progress = !quiet,
    show_col_types = !quiet
  )

  return(tbl)

}

