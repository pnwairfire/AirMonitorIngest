#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Add clustering information to a dataframe
#'
#' @param tbl Tibble with geolocation information (\emph{e.g.} created by
#' \code{wrcc_qualityControl()} or \code{airsis_qualityControl}).
#' @param clusterDiameter Diameter in meters used to determine the number of
#' clusters (see description).
#' @param lonVar Name of longitude variable in the incoming tibble.
#' @param latVar Name of the latitude variable in the incoming tibble.
#' @param maxClusters Maximum number of clusters to try.
#' @param flagAndKeep Logical specifying flagging, rather than removal, of bad
#' data during the QC process.
#'
#' @description Clustering is used to assign individual measurements to
#' deployment locations.
#'
#' A \emph{temporary} monitor will be moved around from time to time, sometimes across the country
#' and sometimes across the street.  We need to assign unique identifiers to each
#' new "deployment" but not when the monitor is moved a short distance.
#'
#' We use clustering to find an appropriate number of unique "deployments".
#' The sensitivity of this algorithm can be adjused with the clusterDiameter argument.
#'
#' Standard \code{kmeans} clustering does not work well when clusters can have widely
#' differing numbers of members. A much better result is acheived with
#' the Partitioning Around Medoids method available in \code{cluster::pam()}.
#'
#' The value of \code{clusterRadius} is compared with the output of
#' \code{cluster::pam(...)$clusinfo[,'av_diss']} to determine the number of clusters.
#'
#'
#' @return Input tibble with additional columns: \code{clusterLon, clusterLat}.
#'
#' @references \href{https://working-with-data.mazamascience.com/2021/07/15/when-k-means-clustering-fails/}{When k-means clustering fails}

addClustering <- function(
  tbl,
  clusterDiameter = 1000,
  lonVar = "longitude",
  latVar = "latitude",
  maxClusters = 50,
  flagAndKeep = FALSE
) {

  logger.debug(" ----- addClustering() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(tbl)

  # Sanity check -- row count
  if ( nrow(tbl) == 0 ) {
    logger.error("Unable to perform clustering: 'tbl' is empty")
    stop(paste0("Unable to perform clustering: 'tbl' is empty"))
  }

  # Sanity check -- names
  if ( !lonVar %in% names(tbl) ) {
    logger.error("No lonVar='%s' column found in 'tbl' tibble with columns: %s", lonVar, paste0(names(tbl), collapse = ", "))
    stop(paste0("Longitudes could not be found.  Did you specify the lonVar argument?"))
  }
  if ( !latVar %in% names(tbl) ) {
    logger.error("No latVar='%s' column found in 'tbl' tibble with columns: %s", latVar, paste0(names(tbl), collapse = ", "))
    stop(paste0("Latitudes could not be found.  Did you specify the latVar argument?"))
  }

  # If we only have a single row, return immediately
  if ( nrow(tbl) == 1 ) {
    tbl$clusterLon <- tbl[[lonVar]][1]
    tbl$clusterLat <- tbl[[latVar]][1]
    tbl$clusterID <- 1
    return(tbl)
  }

  # ----- Clustering -----------------------------------------------------------

  # temporarily remove rows with bad locations if flagAndKeep = TRUE
  if ( flagAndKeep ) {
    # TODO:  Not sure we really need to keep things in order but it can't hurt
    tbl$rowID <- seq_len(nrow(tbl))
    badLocationMask <- is.na(tbl[lonVar]) | is.na(tbl[latVar])
    tbl <- tbl[!badLocationMask,]
    badLocationTbl <- tbl[badLocationMask,]
    # Add new columns to badLocationTbl for rbind
    badLocationTbl$clusterLon <- as.numeric(NA)
    badLocationTbl$clusterLat <- as.numeric(NA)
    badLocationTbl$clusterID <- as.character(NA)
  }

  # NOTE:  A monitor will be moved around from time to time, sometimes across the country
  # NOTE:  and sometimes across the street.  We need to assign unique identifiers to each
  # NOTE:  new "deployment" but not when the monitor is moved a short distance.
  # NOTE:
  # NOTE:  We use clustering to find an appropriate number of unique "deployments".
  # NOTE:  The sensitivity of this algorithm can be adjused with the clusterDiameter argument.
  # NOTE:
  # NOTE:  Standard kmeans clustering does not work well when clusters can have widely
  # NOTE:  differing numbers of members. A much better result is acheived with
  # NOTE:  the Partitioning Around Medoids method available in cluster::pam().
  # NOTE:
  # NOTE:  Try the following example:
  # NOTE:    x <- y <- c(rep(0,3), rep(1,3), rep(10,20), rep(11,20), rep(100,50), rep(101,50))
  # NOTE:    m <- cbind(x,y)
  # NOTE:    layout(matrix(seq(2)))
  # NOTE:    plot(x, y, pch = as.character( stats::kmeans(m,3)$cluster ))
  # NOTE:    plot(x, y, pch = as.character( cluster::pam(m,3)$cluster ))
  # NOTE:    plot(x, y, pch = as.character( cluster::clara(m,3)$cluster ))
  # NOTE:
  # NOTE:  Run the plots a few times and you will see that kmeans clustering sometimes
  # NOTE:  gets it wrong.

  ###logger.trace("Testing up to %s clusters", maxClusters)

  # NOTE:  We need to use cluster::clara when we get above ~2K points.
  # NOTE:  For this reason we need to use clusinfo[,'max_diss'] instead
  # NOTE:  of clusinfo[,'diameter'] as the latter is only provided by
  # NOTE:  cluster::pam and not cluster::clara.
  # NOTE:  (Is there really any difference between 'max_diss' and 'distance'?)

  # Perform clustering
  for ( clusterCount in 1:maxClusters ) {
    if ( nrow(tbl) < 2000 ) {
      ###logger.trace("\ttesting %d clusters using cluster::pam", clusterCount)
      clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)],clusterCount)
    } else {
      ###logger.trace("\ttesting %d clusters using cluster::clara", clusterCount)
      clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)],clusterCount, samples = 50)
    }
    clusterLats <- clusterObj$medoids[,latVar]
    diameters <- 2 * clusterObj$clusinfo[,'max_diss'] # decimal degrees
    # NOTE:  We don't know whether distance is pure NS, EW or some combination
    # NOTE:  so we can't accurately convert to meters. We approximate by
    # NOTE:  assuming a 50-50 split and using 111,320 meters/degree at the equator.
    radianClusterLats <- clusterLats * pi/180
    meters <- diameters * (1 + cos(radianClusterLats))/2 * 111320
    if ( max(meters) < clusterDiameter ) break
  }

  logger.trace("Using %d cluster(s) with a diameter of %d meters", clusterCount, clusterDiameter)

  # Create the vector of deployment identifiers
  if ( nrow(tbl) < 2000 ) {
    clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)], clusterCount)
  } else {
    clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)], clusterCount, samples = 50)
  }

  # Add cluster lons and lats to the tibble
  tbl$clusterLon <- clusterObj$medoids[,lonVar][clusterObj$clustering]
  tbl$clusterLat <- clusterObj$medoids[,latVar][clusterObj$clustering]
  tbl$clusterID <- clusterObj$clustering

  # Reinsert rows with bad locations if flagAndKeep = TRUE
  if ( flagAndKeep ) {
    if ( sum(badLocationMask) > 0 ) {
      # Merge tibbles and sort based on dummy rowID
      tbl <- rbind(tbl, badLocationTbl)
      tbl <- tbl[order(tbl$rowID),]
    }
    tbl$rowID <- NULL
  }

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUG ==================================================================

if ( FALSE ) {

  library(MazamaCoreUtils)
  logger.setup()
  logger.setLevel(TRACE)

  library(AirMonitorIngest)

  tbl <-
    wrcc_downloadData(20150701, 20150930, unitID = 'SM16') %>%
    wrcc_parseData() %>%
    wrcc_qualityControl()

  clusterDiameter = 1000
  lonVar = "GPSLon"
  latVar = "GPSLat"
  maxClusters = 50
  flagAndKeep = FALSE




  newTbl <- addClustering(
    tbl,
    clusterDiameter = clusterDiameter,
    lonVar = lonVar,
    latVar = latVar,
    maxClusters = maxClusters,
    flagAndKeep = flagAndKeep
  )


}
