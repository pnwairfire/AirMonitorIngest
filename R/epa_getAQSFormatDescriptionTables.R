#' @export
#' @import MazamaCoreUtils
#'
#' @title Download EPA Air Quality Format Description Tabless
#'
#' @description
#' Download tables with descriptions of data file formats. These are needed to
#' properly interpret the data files downloaded from EPA. Tables are returned as
#' a named list and include (on 2021-09-24):
#'
#' \itemize{
#' \item{"Site Description"}
#' \item{"Monitor Description"}
#' \item{"Annual Summary"}
#' \item{"Daily Summary"}
#' \item{"Hourly Data"}
#' \item{"8-Hour Average"}
#' \item{"Blanks Data"}
#' }
#'
#' For example, the `epa_parseAQSHourlyData()` function uses the information in
#' the table named "Hourly Data".
#'
#' @param url Character URL for EPA AQS file format descriptions.
#'
#' @return Named list of tables with format descriptions.
#'
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html}{EPA AirData File Formats page}
#'
#' @examples
#' \dontrun{
#' library(AirMonitorIngest)
#'
#' formatDescriptionTables <- epa_getAQSFormatDescriptionTables()
#' head(formatDescriptionTables$`Hourly Data`)
#' }

epa_getAQSFormatDescriptionTables <- function(
  url = 'https://aqs.epa.gov/aqsweb/airdata/FileFormats.html'
) {

  formatDescriptionTables <- MazamaCoreUtils::html_getTables(url)

  names(formatDescriptionTables) <- c(
    "Site Description",
    "Monitor Description",
    "Annual Summary",
    "Daily Summary",
    "Hourly Data",
    "8-Hour Average",
    "Blanks Data"
  )

  return(formatDescriptionTables)

}
