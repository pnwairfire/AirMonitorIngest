#' #' @export
#' #' @importFrom utils read.table
#' #' @importFrom rlang .data
#' #'
#' #' @title Download and parse hourly data from AirNow
#' #'
#' #' @param datestamp Integer or character representing YYYYmmddHH.
#' #' @param baseUrl Base URL for archived hourly data.
#' #' @param quiet Logical passed on to
#' #' \code{readr::read_delim(progress = !quiet)}.
#' #'
#' #' @return Tibble of AirNow hourly data.
#' #'
#' #' @description The \url{https://airnowtech.org} site provides both air
#' #' pollution monitoring data as well as monitoring site location metadata. This
#' #' function retrieves and parses a single, hourly data file and returns it as a
#' #' tibble.
#' #'
#' #' @note As of 2016-12-27, it appears that hourly data are available only for
#' #' 2016 and not for earlier years.
#' #'
#' #' @note Data from locations whose timezones have a fractional offset from UTC
#' #' are removed as the AirMonitor data model only supports data reported on hour
#' #' boundaries. As of 2019-06-26, this only applies to US Department of State
#' #' monitors in Myanmar, Sri Lanka, India and Nepal.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' tbl <- airnow_getHourlyData(2021101200)
#' #' }
#'
#' airnow_getHourlyData <- function(
#'   datestamp = NULL,
#'   baseUrl = "https://files.airnowtech.org/airnow",
#'   quiet = TRUE
#' ) {
#'
#'   if ( logger.isInitialized() )
#'     logger.debug(" ----- epa_aqs_getSites() ----- ")
#'
#'   # ----- Validate Parameters --------------------------------------------------
#'
#'   MazamaCoreUtils::stopIfNull(datestamp)
#'   MazamaCoreUtils::stopIfNull(baseUrl)
#'
#'   datestamp <- as.character(datestamp)
#'
#'   if ( stringr::str_length(datestamp) != 10 )
#'     stop("'datestamp' must be in YYYYmmddHH form")
#'
#'   # ----- Download data --------------------------------------------------------
#'
#'   # Strip off any final "/"
#'   baseUrl <- stringr::str_replace(baseUrl, "/$", "")
#'
#'   # Create URL
#'   datestamp <- as.character(datestamp)
#'   year <- stringr::str_sub(datestamp,1,4)
#'   ymd <- stringr::str_sub(datestamp,1,8)
#'   url <- paste0(baseUrl, "/", year, "/", ymd, "/HourlyData_", datestamp, ".dat")
#'
#'   col_names <- c(
#'     "ValidDate", "ValidTime", "AQSID", "SiteName", "GMTOffset",
#'     "ParameterName", "ReportingUnits", "Value", "DataSource"
#'   )
#'
#'   col_types <- paste0("ccccd", "ccdc")
#'
#'   # # NOTE:  TODO:  Do we need to retain this encoding for older data?
#'   # # NOTE:
#'   # # NOTE:  12/26/16|00:00|000051501|Z<82>phirin|-5|OZONE|PPB|39|Meteorological Service of Canada
#'   #
#'   # locale <- readr::locale(encoding = "CP437")
#'
#'   # Read in text as a dataframe
#'   tbl <-
#'     readr::read_delim(
#'       file = url,
#'       delim = "|",
#'       col_names = col_names,
#'       col_types = col_types,
#'       progress = !quiet
#'       ###locale = locale
#'     ) %>%
#'     # Remove records with fractional hours (see @note above)
#'     dplyr::filter(stringr::str_detect(.data$ValidTime, ":00"))
#'
#'   return(tbl)
#'
#' }
