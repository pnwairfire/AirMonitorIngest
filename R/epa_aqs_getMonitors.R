#' @export
#' @import MazamaCoreUtils
#'
#' @title Download and parse EPA AQS monitor metadata
#'
#' @description
#' Download monitor metadata from the US EPA and save it to a directory. Then
#' uncompress and parse the data into a tibble.
#'
#' @param downloadDir Directory where .zip file will be saved.
#' @param baseUrl Character base URL for the EPA AQS archive.
#' @param quiet Logical passed on to \code{utils::download.file()}.
#'
#' @return Tibble of EPA site metadata.
#'
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Meta}{Site and Monitor Descriptions}
#'
#' @examples
#' \dontrun{
#' library(AirMonitorIngest)
#'
#' # Create a directory specifically for EPA data
#' dir.create("~/Data/EPA", recursive = TRUE)
#'
#' # Set logging level so messages and errors will appear in the console
#' MazamaCoreUtils::initializeLogging(logDir = "~/Data/EPA/")
#' logger.setLevel(TRACE)
#'
#' # Download and parse site metadata
#' AQS_monitors <- epa_aqs_getMonitors(downloadDir = "~/Data/EPA", quiet = FALSE)
#' }

epa_aqs_getMonitors <- function(
  downloadDir = tempdir(),
  baseUrl = 'https://aqs.epa.gov/aqsweb/airdata/',
  quiet = TRUE
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- epa_aqs_getMonitors() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(downloadDir)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Download data --------------------------------------------------------

  # Set up file names and paths
  url <- paste0(baseUrl, "aqs_monitors.zip")
  zipFile <- path.expand( file.path(downloadDir, "aqs_monitors.zip") )

  if ( !file.exists(zipFile) ) {

    if ( logger.isInitialized() )
      logger.trace('Downloading %s ...', zipFile)

    result <- try({
      utils::download.file(url, zipFile, quiet = quiet)
    }, silent = quiet)

    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( logger.isInitialized() )
        logger.error(err_msg)
      stop(err_msg)
    }

    if ( logger.isInitialized() )
      logger.trace(paste0('Finished downloading.'))

  }

  # ----- Uncompress zip file --------------------------------------------------

  csvFile <- stringr::str_replace(zipFile, "\\.zip", "\\.csv")

  # Uncompress
  if ( logger.isInitialized() )
    logger.trace("Uncompressing %s ...", zipFile)

  utils::unzip(zipFile, exdir = dirname(zipFile))

  if ( logger.isInitialized() )
    logger.trace(paste0("Finished uncompressing."))

  # ----- Parse csv file -------------------------------------------------------

  # Here are the column names:

  #   > dplyr::glimpse(tbl, width = 75)
  #   Rows: 356,777
  #   Columns: 32
  #   $ `State Code`                   <chr> "01", "01", "01", "01", "01", "01"
  #   $ `County Code`                  <chr> "001", "001", "001", "001", "001",
  #   $ `Site Number`                  <chr> "0001", "0001", "0002", "0002", "0
  #   $ `Parameter Code`               <dbl> 11103, 42401, 42401, 44201, 44201,
  #   $ `Parameter Name`               <chr> "Benzene soluble organics (TSP)",

  #   $ POC                            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  #   $ Latitude                       <dbl> 32.43746, 32.43746, 32.42847, 32.4
  #   $ Longitude                      <dbl> -86.47289, -86.47289, -86.44358, -
  #   $ Datum                          <chr> "WGS84", "WGS84", "WGS84", "WGS84"
  #   $ `First Year of Data`           <dbl> 1974, 1974, 1980, 1980, 1989, 1959

  #   $ `Last Sample Date`             <date> 1974-06-10, 1976-08-16, 1982-07-3
  #   $ `Monitor Type`                 <chr> "OTHER", "OTHER", "SLAMS", "SLAMS"
  #   $ Networks                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA
  #   $ `Reporting Agency`             <chr> NA, "Al Dept Of Env Mgt", "Al Dept
  #   $ PQAO                           <chr> NA, "Al Dept Of Env Mgt", "Al Dept

  #   $ `Collecting Agency`            <chr> NA, "Al Dept Of Env Mgt", "Al Dept
  #   $ Exclusions                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA
  #   $ `Monitoring Objective`         <chr> "UNKNOWN", "HIGHEST CONCENTRATION"
  #   $ `Last Method Code`             <chr> "091", "091", "020", "011", "014",
  #   $ `Last Method`                  <chr> "HI-VOL - BENZENE EXTRACTION-SOXHL

  #   $ `Measurement Scale`            <chr> NA, "NEIGHBORHOOD", NA, "NEIGHBORH
  #   $ `Measurement Scale Definition` <chr> NA, "500 M TO 4KM", NA, "500 M TO
  #   $ `NAAQS Primary Monitor`        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA
  #   $ `QA Primary Monitor`           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA
  #   $ `Local Site Name`              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA

  #   $ Address                        <chr> "KING ARTHUR TRAILER COURT, PRATTV
  #   $ `State Name`                   <chr> "Alabama", "Alabama", "Alabama", "
  #   $ `County Name`                  <chr> "Autauga", "Autauga", "Autauga", "
  #   $ `City Name`                    <chr> "Prattville", "Prattville", "Pratt
  #   $ `CBSA Name`                    <chr> "Montgomery, AL", "Montgomery, AL"

  #   $ `Tribe Name`                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA
  #   $ `Extraction Date`              <date> 2021-05-18, 2021-05-18, 2021-05-1

  # Assign appropriate data types
  col_types <- paste0("ccccc", "cddcd", "ccccc", "ccccc", "ccccc", "ccccc", "cc")

  # Read in the data
  if ( logger.isInitialized() )
    logger.trace('Parsing %s ...', csvFile)

  result <- try({
    tbl <- readr::read_csv(csvFile, col_types = col_types, progress = !quiet)
  }, silent = quiet)

  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    if ( logger.isInitialized() )
      logger.error(err_msg)
    stop(err_msg)
  }

  if ( logger.isInitialized() )
    logger.trace('Finished parsing.')

  # Cleanup
  file.remove(csvFile)

  return(tbl)

}
