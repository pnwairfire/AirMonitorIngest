#' @export
#'
#' @title Leaflet Interactive Map for Known Locations
#'
#' @param tbl Tibble of known locations
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#'
#' @description This function creates interactive maps that will be displayed in
#'   RStudio's 'Viewer' tab.
#'
#' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile"
#'   names. Current mappings include:
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the
#' underlying map tile if available. See
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of
#' "provider tiles" to use as the background map.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'

table_leaflet <- function(
  locationTbl = NULL,
  maptype = "terrain"
) {

  radius <- 10
  col <- "blue"
  opacity <- 0.5

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(maptype)

  # ----- Figure out names for a legend and colors for each point --------------

  # Ignore warnings from RColorBrewer as leaflet::colorBin does the right thing

  # Create popupText
  locationTbl$popupText <- paste0(
    "<b>", locationTbl$locationName, "</b><br/>",
    "location = ", locationTbl$longitude, " to ", "latitude = ", locationTbl$latitude, "<br/>",
    "from  ", locationTbl$AQS_Site.Established.Date, " to ", locationTbl$AQS_Site.Closed.Date, "<br/>",
    "timezone = ", locationTbl$timezone, "<br/>",
    "ISO = ", locationTbl$countryCode, ".", locationTbl$stateCode, "<br/>",
    "county = ", locationTbl$county, "<br/>",
    "address = ", locationTbl$houseNumber, ", ", locationTbl$street, ", ", locationTbl$city, ", ", locationTbl$zip, "<br/>",
    "AQSID = ", locationTbl$AQS_AQSID, "<br/>",
    "agency = ", locationTbl$AQS_Owning.Agency, "<br/>"
  )

  # Extract view information
  lonRange <- range(locationTbl$longitude, na.rm = TRUE)
  latRange <- range(locationTbl$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange),diff(latRange), na.rm = TRUE)
  # Determine appropriate zoom level
  if (maxRange > 20) {
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }

  # Convert locations to SpatialPointsDataFrame
  locationTbl <- locationTbl[!is.na(locationTbl$latitude),]
  SPDF <- sp::SpatialPointsDataFrame(coords = cbind(locationTbl$longitude,locationTbl$latitude),
                                     data = as.data.frame(locationTbl))

  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == 'terrain') {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }

  # Create leaflet map
  m <- leaflet::leaflet(SPDF) %>%
    leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      radius = radius,
      fillColor = col,
      fillOpacity = opacity,
      stroke = FALSE,
      popup = locationTbl$popupText,
      layerId = locationTbl$locationID
    )

  # ----- Return ---------------------------------------------------------------

  return(m)

}
