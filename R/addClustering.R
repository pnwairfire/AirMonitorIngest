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
  # NOTE:  Use as.numeric() to remove any names associated with these vectors
  if ( nrow(tbl) == 1 ) {
    tbl$clusterLon <- as.numeric(tbl[[lonVar]][1])
    tbl$clusterLat <- as.numeric(tbl[[latVar]][1])
    tbl$clusterID <- 1
    return(tbl)
  }

  # ----- Separate table (flagAndKeep) -----------------------------------------

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

  # ----- Cluster by distance --------------------------------------------------

  tbl <-
    clusterByDistance(
      tbl,
      clusterDiameter = clusterDiameter,
      lonVar = lonVar,
      latVar = latVar,
      maxClusters = maxClusters
    )

  # ----- Verify clustering ----------------------------------------------------

  # Only use one locaion record per clusterID
  distinctTbl <-
    tbl %>%
    dplyr::distinct(.data$clusterID, .keep_all = TRUE) %>%
    dplyr::mutate(
      longitude = .data$clusterLon,
      latitude = .data$clusterLat
    ) %>%
    dplyr::select(c("longitude", "latitude"))

  # Check if any of the clustered locations are too close
  adjacentDistances <-
    distinctTbl %>%
    MazamaLocationUtils::table_findAdjacentDistances(
      distanceThreshold = clusterDiameter
    )

  # ----- Fix clustering -------------------------------------------------------

  if ( nrow(adjacentDistances) > 0 ) {

    # NOTE:  Clearly, clustering has failed. At this point we just cluster the
    # NOTE:  distinct locations. Having just a few points should radically
    # NOTE:  improve the reliability of cluster::pam().

    # Cluster the distinct locations
    distinctTbl <-
      clusterByDistance(
        distinctTbl,
        clusterDiameter = clusterDiameter,
        lonVar = "longitude",
        latVar = "latitude",
        maxClusters = maxClusters
      )

    clusterCount <- max(as.numeric(distinctTbl$clusterID), na.rm = TRUE)

    if ( clusterCount < nrow(distinctTbl) ) {
      logger.trace(
        "Cluster distance check: reducing clusters from %d to %d",
        nrow(distinctTbl),
        clusterCount
      )
    }

    # Now use this clusterCount for full clustering -- see clusterByDistance()

    if ( nrow(tbl) < 2000 ) {
      clusterObj <- cluster::pam(tbl[,c(lonVar,latVar)], clusterCount)
    } else {
      clusterObj <- cluster::clara(tbl[,c(lonVar,latVar)], clusterCount, samples = 50)
    }

    tbl$clusterLon <- as.numeric(clusterObj$medoids[,lonVar][clusterObj$clustering])
    tbl$clusterLat <- as.numeric(clusterObj$medoids[,latVar][clusterObj$clustering])
    tbl$clusterID <- as.character(clusterObj$clustering)

  }

  # ------ Recombine table (flagAndKeep) ---------------------------------------

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

  # Also try wrcc_downloadData(20210101, 20220101, unitID = "sm23")
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
