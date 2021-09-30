#' @export
#'
#' @title Browse EPA AQS web pages
#'
#' @description
#' Open the relevant EPA AQS web pages in the default web browser. These pages
#' need to be regularly consulted when working with AQS data and it is helpful
#' to open them quickly.
#'
#' \itemize{
#' \item{\url{https://aqs.epa.gov/aqsweb/airdata/download_files.html}}
#' \item{\url{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html}}
#' \item{\url{https://www.epa.gov/criteria-air-pollutants/naaqs-table}}
#' \item{\url{https://www.epa.gov/outdoor-air-quality-data/interactive-map-air-quality-monitors}}
#' }
#'

epa_aqs_browse <- function() {

  utils::browseURL("https://aqs.epa.gov/aqsweb/airdata/download_files.html")
  utils::browseURL("https://aqs.epa.gov/aqsweb/airdata/FileFormats.html")
  utils::browseURL("https://www.epa.gov/criteria-air-pollutants/naaqs-table")
  utils::browseURL("https://www.epa.gov/outdoor-air-quality-data/interactive-map-air-quality-monitors")

}
