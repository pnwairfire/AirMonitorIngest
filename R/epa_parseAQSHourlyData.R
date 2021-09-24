#' @export
#' @import MazamaCoreUtils
#'
#' @title Parse Hourly EPA Air Quality Data
#'
#' @description
#' Uncompresses previously downloaded, hourly air quality .zip files from the
#' US EPA and parse the data into a tibble.
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
#' CSV files are removed after data are read into a tibble.
#'
#' @param zipFile Absolute path to monitoring data .zip file
#' @param quiet Logical passed on to \code{readr::read_csv(progress = !quiet)}.
#'
#' @return Tibble of EPA data.
#'
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_hourly_data_files}{file format description}
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
#' # Save the download in ~/Data/EPA
#' zipFile <- epa_downloadAQSHourlyData(2008, "88101", "~/Data/EPA/", quiet = FALSE)
#'
#' # Uncompress and parse into a tibble
#' tbl <- epa_parseAQSHourlyData(zipFile)
#' }

epa_parseAQSHourlyData <- function(
  zipFile = NULL,
  quiet = TRUE
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- epa_parseAQSHourlyData() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(zipFile)

  if ( !file.exists(zipFile) ) {
    err_msg <- sprintf("zipFile not found: %s")
    if ( logger.isInitialized() ) logger.error(err_msg)
    stop(err_msg)
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

  # Here are the column names from epa_downloadAQSData(1980, "88101"):

  #   > dplyr::glimpse(tbl, width = 75)
  #   Rows: 70,043
  #   Columns: 24
  #   $ `State Code`          <chr> "06", "06", "06", "06", "06", "06", "06", "
  #   $ `County Code`         <chr> "019", "019", "019", "019", "019", "019", "
  #   $ `Site Num`            <chr> "5001", "5001", "5001", "5001", "5001", "50
  #   $ `Parameter Code`      <chr> "88101", "88101", "88101", "88101", "88101"
  #   $ POC                   <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3"

  #   $ Latitude              <dbl> 36.81945, 36.81945, 36.81945, 36.81945, 36.
  #   $ Longitude             <dbl> -119.7164, -119.7164, -119.7164, -119.7164,
  #   $ Datum                 <chr> "NAD83", "NAD83", "NAD83", "NAD83", "NAD83"
  #   $ `Parameter Name`      <chr> "PM2.5 - Local Conditions", "PM2.5 - Local
  #   $ `Date Local`          <chr> "2008-11-25", "2008-11-25", "2008-11-25", "

  #   $ `Time Local`          <chr> "16:00", "17:00", "18:00", "19:00", "20:00"
  #   $ `Date GMT`            <chr> "2008-11-26", "2008-11-26", "2008-11-26", "
  #   $ `Time GMT`            <chr> "00:00", "01:00", "02:00", "03:00", "04:00"
  #   $ `Sample Measurement`  <dbl> 64, 62, 50, 50, 63, 72, 77, 64, 73, 73, 69,
  #   $ `Units of Measure`    <chr> "Micrograms/cubic meter (LC)", "Micrograms/

  #   $ MDL                   <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
  #   $ Uncertainty           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ Qualifier             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ `Method Type`         <chr> "FEM", "FEM", "FEM", "FEM", "FEM", "FEM", "
  #   $ `Method Code`         <chr> "170", "170", "170", "170", "170", "170", "

  #   $ `Method Name`         <chr> "Met One BAM-1020 Mass Monitor w/VSCC - Bet
  #   $ `State Name`          <chr> "California", "California", "California", "
  #   $ `County Name`         <chr> "Fresno", "Fresno", "Fresno", "Fresno", "Fr
  #   $ `Date of Last Change` <chr> "2010-09-16", "2010-09-16", "2010-09-16", "

  # Assign appropriate data types
  col_types <- paste0("ccccc", "ddccc", "cccdc", "ddccc", "cccc")

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
