#' @export
#'
#' @title Browse AirNow web pages
#'
#' @description
#' Open the relevant AirNow web pages in the default web browser. These pages
#' need to be regularly consulted when working with AirNow data and it is helpful
#' to open them quickly.
#'
#' \itemize{
#' \item{\url{https://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf}}
#' \item{\url{https://docs.airnowapi.org/docs}}
#' \item{\url{https://docs.airnowapi.org/files}}
#' }
#'

airnow_openWebPages <- function() {

  utils::browseURL("https://www.airnowapi.org/docs/MonitoringSiteFactSheet.pdf")
  utils::browseURL("https://docs.airnowapi.org/docs")
  utils::browseURL("https://docs.airnowapi.org/files")

}
