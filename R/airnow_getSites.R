#' @export
#'
#' @title Download and parse AirNow sites metadata
#'
#' @param baseUrl URL of the AirNow monitoring site locations file.
#' @param sitesFile Name of the AirNow monitoring site locations file.
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
#' airnow_sites <- airnow_getSites()
#' }

airnow_downloadSites <- function(
  baseUrl = "https://files.airnowtech.org/airnow/today/",
  sitesFile = "monitoring_site_locations.dat",
  quiet = TRUE
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_getSites() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(sitesFile)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Download data --------------------------------------------------------

  url <- paste0(baseUrl, sitesFile)

  # NOTE:  Information on the structure of this file come from the Monitoring
  # NOTE:  Site Factsheet (June, 2019).
  # NOTE:    https://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf
  # NOTE:
  # NOTE:  THIS INFORMATION IS INCORRECT!
  # NOTE:
  # NOTE:  The column names below are correct.

  col_names <- c(
    "AQSID", "parameterName", "siteCode", "siteName", "status",
    "agencyID", "agencyName", "EPARegion", "latitude", "longitude",
    "elevation", "GMTOffsetHours", "countryCode", "FIPSCMSACode", "CMSAName",
    "FIPSMSACode", "MSAName", "FIPSStateCode", "stateCode", "GNISCountyCode",
    "countyName", "GNISCityCode", "cityName"
    )

  col_types <- paste0("ccccc", "cccdd", "ddccc", "ccccc", "ccc")


  # NOTE:  This plain text file is not properly encoded:
  # NOTE:    800090033         PM2.5     0033                             Coyoac\xa0n

  # Fix the encoding
  locale <- readr::locale(encoding = "CP437")

  # Read in text as a dataframe
  tbl <- readr::read_delim(
    url,
    delim = "|",
    col_names = col_names,
    col_types = col_types,
    locale = locale,
    progress = !quiet
  )

  return(tbl)

}

