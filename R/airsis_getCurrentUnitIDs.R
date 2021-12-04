#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Get AIRSIS unit IDs from the daily summary page
#'
#' @param url URL with daily summary links. (NOTE: only file paths are supported.)
#'
#' @description Parse the AIRSIS Current Status page for a particular provider
#' to extract unit IDs for those monitors that are currently reporting.
#'
#' @return Vector of AIRSIS unit IDs.
#'

airsis_getCurrentUnitIDs <- function(
  url = "~/Downloads/apcd_stattable.aspx.html"
) {

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- airsis_getCurrentUnitIDs() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(url)

  # ----- Get unit IDs ---------------------------------------------------------

  # Times
  timezone = "UTC"
  now <- lubridate::now(tzone = timezone)
  startdate <- lubridate::floor_date(now, unit = "day") - lubridate::ddays(30)

  # NOTE:   Use chrome devtools to review the web page for link construction

  status <-
    MazamaCoreUtils::html_getTable(url, index = 4) %>%
    dplyr::slice(-c(1,2)) %>%
    dplyr::select(c(1,2,3))

  names(status) <- c("alias", "Date.Time", "location")

  # NOTE:  We are forcing the "Date/Time" column to UTC rather than local time.
  # NOTE:  But it won't make any difference because we are looking for anything
  # NOTE:  in the past 30 days.

  status <-
    status %>%
    dplyr::mutate(
      location = stringr::str_trim(.data$location),
      datetime = lubridate::mdy_hm(.data$Date.Time, tz = "UTC")
    ) %>%
    dplyr::filter(.data$datetime > !!startdate) %>%
    dplyr::select(-.data$Date.Time)

  # > head(status)
  # # A tibble: 6 × 3
  #   alias           location                                         datetime
  #   <chr>           <chr>                                            <dttm>
  # 1 NPT1002         "Lat: 46° 24' 0.34\" N, Lon: 116° 48' 11.77\""   2021-11-29 19:00:00
  # 2 NPT1003         "Lat: 46° 24' 0.06\" N, Lon: 116° 48' 12.1\""    2021-11-29 19:00:00
  # 3 SISK1003 (Weed) "Lat: 41° 25' 37.81\" N, Lon: 122° 22' 58.62"    2021-12-03 22:00:00
  # 4 SISK1004        "Lat: 41° 47' 2.98\" N, Lon: 122° 3' 5.7\" W"    2021-12-03 22:00:00
  # 5 SISK1005        "Lat: 41° 31' 42.34\" N, Lon: 122° 22' 18.3\""   2021-12-03 22:00:00
  # 6 YOSE EBAM 002   "8162-8166 Chilnualna Rd, California 95389, USA" 2021-12-03 22:00:00

  links <-
    MazamaCoreUtils::html_getLinks(url) %>%
    dplyr::filter(stringr::str_detect(.data$linkUrl, "^map.aspx\\?id=\\S+")) %>%
    dplyr::mutate(
      location = stringr::str_trim(.data$linkName),
      unitID = stringr::str_sub(.data$linkUrl, 13, -1)
    ) %>%
    dplyr::select(-.data$linkName)

  # > head(links)
  # # A tibble: 6 × 3
  #   location                                       linkUrl          unitID
  #   <chr>                                          <chr>            <chr>
  # 1 "Denver, Colorado"                             map.aspx?id=1    1
  # 2 "Colorado"                                     map.aspx?id=2    2
  # 3 "Front Range"                                  map.aspx?id=1002 1002
  # 4 "Lat: 39° 30' 30.49\" N, Lon: 121° 35' 25.25"  map.aspx?id=1029 1029
  # 5 "Lat: 32° 48' 38.09\" N, Lon: 117° 7' 16.35\"" map.aspx?id=1050 1050
  # 6 "Lat: 42° 29' 38.6\" N, Lon: 114° 24' 10.39\"" map.aspx?id=1004 1004


  currentUnitIDs <-
    dplyr::left_join(
      status,
      links,
      by = "location"
    ) %>%
    dplyr::pull(.data$unitID) %>%
    sort() %>% unique()


  # ----- Return ---------------------------------------------------------------

  return(currentUnitIDs)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  url <- "~/Downloads/arb3_stattable.aspx.html"



}
