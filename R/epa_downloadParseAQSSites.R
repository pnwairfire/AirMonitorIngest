#' @export
#' @import MazamaCoreUtils
#'
#' @title Download Hourly EPA Air Quality Data
#'
#' @description
#' Download hourly air quality data from the US EPA and save it to a directory.
#'
#' EPA parameter codes and (start year) include:
#'
#' \enumerate{
#' \item{44201}{ -- Ozone (1980)}
#' \item{42401}{ -- SO2 (1980)}
#' \item{42101}{ -- CO (1980)}
#' \item{42602}{ -- NO2 (1980)}
#' \item{88101}{ -- PM2.5 FRM/FEM (2008)}
#' \item{88502}{ -- PM2.5 non FRM/FEM (1998)}
#' \item{81102}{ -- PM10 (1988)}
#' \item{SPEC}{ -- PM2.5 Speciation(2001)}
#' \item{PM10SPEC}{ -- PM10 Speciation (1988)}
#' \item{WIND}{ -- Winds (1980)}
#' \item{TEMP}{ -- Temperature (1980)}
#' \item{PRESS}{ -- Barometric Pressure (1980)}
#' \item{RH_DP}{ -- RH and Dewpoint (1980)}
#' \item{HAPS}{ -- HAPs (1993)}
#' \item{VOCS}{ -- VOCs (1980)}
#' \item{NONOxNOy (1980)}
#' }
#'
#' @note Unzipped CSV files are almost 100X larger than the compressed .zip files.
#'
#' @param downloadDir Directory where monitoring data .zip file will be saved.
#' @param baseUrl Character base URL for EPA AQS archive.
#' @param quiet Logical passed on to \code{utils::download.file()}.
#'
#' @return Filepath of the downloaded zip file.
#'
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
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
#' AQS_sites <- epa_downloadParseAQSSites(downloadDir = "~/Data/EPA", quiet = FALSE)
#' }

epa_downloadParseAQSSites <- function(
  downloadDir = tempdir(),
  baseUrl = 'https://aqs.epa.gov/aqsweb/airdata/',
  quiet = TRUE
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- epa_downloadParseAQSSites() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(downloadDir)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Download data --------------------------------------------------------

  # Set up file names and paths
  url <- paste0(baseUrl, "aqs_sites.zip")
  zipFile <- path.expand( file.path(downloadDir, "aqs_sites.zip") )

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
  #   Rows: 20,730
  #   Columns: 28
  #   $ `State Code`            <chr> "01", "01", "01", "01", "01", "01", "01",
  #   $ `County Code`           <chr> "001", "001", "001", "003", "003", "003",
  #   $ `Site Number`           <chr> "0001", "0002", "0003", "0001", "0002", "
  #   $ Latitude                <dbl> 32.43746, 32.42847, 32.33266, 0.00000, 30
  #   $ Longitude               <dbl> -86.47289, -86.44358, -86.79152, 0.00000,

  #   $ Datum                   <chr> "WGS84", "WGS84", "WGS84", "NAD27", "WGS8
  #   $ Elevation               <dbl> 64.00, 0.00, 41.00, 0.00, 0.00, 49.00, 37
  #   $ `Land Use`              <chr> "RESIDENTIAL", "AGRICULTURAL", "FOREST",
  #   $ `Location Setting`      <chr> "SUBURBAN", "RURAL", "RURAL", "RURAL", "R
  #   $ `Site Established Date` <date> 1974-05-01, 1980-01-01, 1989-08-31, 1959

  #   $ `Site Closed Date`      <date> 1976-12-31, 1982-12-31, 1990-11-30, 1959
  #   $ `Met Site State Code`   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  #   $ `Met Site County Code`  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  #   $ `Met Site Site Number`  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  #   $ `Met Site Type`         <chr> NA, NA, NA, NA, NA, NA, "ON-SITE MET EQUI

  #   $ `Met Site Distance`     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  #   $ `Met Site Direction`    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  #   $ `GMT Offset`            <dbl> -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -
  #   $ `Owning Agency`         <chr> "Al Dept Of Env Mgt", "Al Dept Of Env Mgt
  #   $ `Local Site Name`       <chr> NA, NA, NA, NA, NA, NA, "FAIRHOPE, Alabam

  #   $ Address                 <chr> "KING ARTHUR TRAILER COURT, PRATTVILLE,AL
  #   $ `Zip Code`              <chr> "36067", NA, "36003", NA, "36567", NA, "3
  #   $ `State Name`            <chr> "Alabama", "Alabama", "Alabama", "Alabama
  #   $ `County Name`           <chr> "Autauga", "Autauga", "Autauga", "Baldwin
  #   $ `City Name`             <chr> "Prattville", "Prattville", "Not in a Cit

  #   $ `CBSA Name`             <chr> "Montgomery, AL", "Montgomery, AL", "Mont
  #   $ `Tribe Name`            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N
  #   $ `Extraction Date`       <date> 2021-05-18, 2021-05-18, 2021-05-18, 2021

  # Assign appropriate data types
  col_types <- paste0("cccdd", "cdccc", "ccccc", "dcdcc", "ccccc", "ccc")

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
    logger.trace('Parsing %s', csvFile)

  # Cleanup
  file.remove(csvFile)

  return(tbl)

}
