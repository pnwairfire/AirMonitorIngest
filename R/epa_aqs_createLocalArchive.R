#' @export
#' @import MazamaCoreUtils
#' @importFrom rlang .data
#'
#' @title Create a 'data' dataframe with required data
#'
#' @description
#' Create a local archive of  EPA AQS transofrmed into data appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' The data model is that data are stored in a tibble named \code{data} with an initial
#' \code{datetime} column and monitor-specific data in columns identified with a
#' \code{deviceDeploymentID} that matches one found in the incoming \code{meta} table.
#' Both \code{data} and \code{meta} will be saved as `.rda`, `.csv` and `.csv.gz`
#' files for maximum portability to other languages like python, javascript or
#' even to desktop spreadsheet software.
#'
#' The archive structure underneath \code{archiveBaseDir} will be:
#'
#' \preformatted{
#' <archiveBaseDir>/EPA_AQS/<parameterCode>/<year>/
#' }
#'
#' Files will be named as:
#'
#' \preformatted{
#' epa_aqs_<parameterCode>_<year>_meta.[rda|csv|csv.gz]
#' epa_aqs_<parameterCode>_<year>_data.[rda|csv|csv.gz]
#' }
#'
#' @param sites_locationTbl EPA AQS "known locations" table for \code{parameterCode} monitors.
#' @param downloadDir Directory in which to save downloaded EPA .zip files. If \code{NULL}, a temp directory is used.
#' @param parameterCode EPA "Parameter Code".
#' @param years Vector of years for which to transform data.
#' @param archiveBaseDir Path under which the directory structure will be created.
#' @param quiet Logical passed on to \code{utils::download.file()}.
#'
#' @return Returns 0 on success.
#'

epa_aqs_createLocalArchive <- function(
  sites_locationTbl = NULL,
  downloadDir = NULL,
  parameterCode = NULL,
  years = NULL,
  archiveBaseDir = NULL,
  quiet = FALSE
) {

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- epa_aqs_createLocalArchive() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(sites_locationTbl)
  MazamaCoreUtils::stopIfNull(parameterCode)
  MazamaCoreUtils::stopIfNull(years)
  MazamaCoreUtils::stopIfNull(archiveBaseDir)

  # TODO:  Validate parameterCode and years

  if ( !dir.exists(path.expand(archiveBaseDir)) ) {
    err_msg <- sprintf("'archiveBaseDir: %s does not exist. Please create it.")
    if ( MazamaCoreUtils::logger.isInitialized() )
      logger.error(err_msg)
    stop(err_msg)
  }

  # ----- Year loop ------------------------------------------------------------

  for ( year in years ) {

    result <- try({

      # NOTE:  We choose not to pipe these one to the next so that we can see
      # NOTE:  all logging messages in the proper order.

      # * create 'meta' first -----

      # Download/parse monitor metadata
      AQS_monitors <-
        epa_aqs_getMonitors(
          downloadDir = downloadDir,
          quiet = quiet
        )

      # Convert into 'meta'
      meta <-
        epa_aqs_createMeta(
          AQS_monitors,
          sites_locationTbl = sites_locationTbl,
          parameterCode = parameterCode
        )

      # * create 'data' second -----

      # Download hourly data

      result <- try({

        zipFile <-
          epa_aqs_downloadHourlyData(
            year = year,
            parameterCode = parameterCode,
            downloadDir = downloadDir,
            quiet = quiet
          )

      }, silent = quiet)

      if ( "try-error" %in% class(result) ) {
        err_msg <- sprintf("Skipping %d because no data is available", year)
        logger.warn(err_msg)
        next
      }

      # Uncompress and parse into a tibble
      AQS_data <-
        epa_aqs_parseHourlyData(
          zipFile,
          quiet = quiet
        )

      # Convert into 'data'
      data <-
        epa_aqs_createData(
          AQS_data,
          meta = meta,
          parameterCode = parameterCode
        )

      # * then subset 'meta' -----

      # Limit 'meta' to only those monitors found in 'data'

      data_IDs <- names(data)[2:ncol(data)]

      meta <- dplyr::filter(meta, .data$deviceDeploymentID %in% data_IDs)

      # * finally, reorder 'data' -----

      data <- dplyr::select(data, c('datetime', meta$deviceDeploymentID))

      if ( !all(c('datetime', meta$deviceDeploymentID) == names(data)) ) {
        err_msg <- sprintf("Skipping %d because meta$deviceDeploymentID does not match names(data)", year)
        logger.warn(err_msg)
        next
      }

      # ----- Save files -------------------------------------------------------

      basePath <- file.path(path.expand(archiveBaseDir), "epa_aqs", parameterCode, year)

      dir.create(basePath, showWarnings = FALSE, recursive = TRUE)

      fileBase <- sprintf("epa_aqs_%s_%s_", parameterCode, year)

      base <- file.path(basePath, fileBase)

      # * meta -----

      save(
        meta,
        file = paste0(base, "meta.rda")
      )

      readr::write_csv(
        meta,
        file = paste0(base, "meta.csv"),
        na = "",
        quote = "all"
      )

      readr::write_csv(
        meta,
        file = paste0(base, "meta.csv.gz"),
        na = "",
        quote = "all"
      )

      # * data -----

      save(
        data,
        file = paste0(base, "data.rda")
      )

      readr::write_csv(
        data,
        file = paste0(base, "data.csv"),
        na = "",
        quote = "all"
      )

      readr::write_csv(
        data,
        file = paste0(base, "data.csv.gz"),
        na = "",
        quote = "all"
      )

    }, silent = FALSE)

    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      logger.error(err_msg)
    }

    # ----- Clear memory -------------------------------------------------------

    rm(list = c("AQS_monitors", "AQS_data", "meta", "data"))
    dummy <- gc(verbose = !quiet)

    if ( MazamaCoreUtils::logger.isInitialized() )
      logger.trace("finished creating %s", fileBase)

  }

  if ( MazamaCoreUtils::logger.isInitialized() ) {
    logger.debug("========================================")
    logger.debug("Finished creating all years!")
  }

  return(invisible(0))

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(MazamaCoreUtils)

  logDir <- "~/Data/monitoring/epa_aqs/88502"
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)
  MazamaCoreUtils::initializeLogging(logDir)
  MazamaCoreUtils::logger.setLevel(DEBUG)

  library(MazamaLocationUtils)
  MazamaLocationUtils::mazama_initialize()
  MazamaLocationUtils::setLocationDataDir("~/Data/monitoring/known_locations")

  sites_locationTbl <- MazamaLocationUtils::table_load("AQS_88502_sites")

  downloadDir <- "~/Data/EPA"
  parameterCode <- "88101"
  year <- 2016
  archiveBaseDir <- "~/Data/monitoring"
  quiet <- FALSE


  library(AirMonitorIngest)

  epa_aqs_createLocalArchive(
    sites_locationTbl = sites_locationTbl,
    downloadDir = "~/Data/EPA",
    parameterCode = "88502",
    years = 1980:2021,
    archiveBaseDir = "~/Data/monitoring",
    quiet = FALSE
  )







}

