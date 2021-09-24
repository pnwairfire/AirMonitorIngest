#' @export
#' @import MazamaCoreUtils
#'
#' @title Parse EPA Air Quality Data
#'
#' @description This function uncompresses previously downloaded air quality .zip
#' files from the EPA and parses that d into a tibble.
#'
#' Available parameters include:
#'
#' \enumerate{
#' \item{Ozone}
#' \item{SO2}
#' \item{CO}
#' \item{NO2}
#' \item{PM2.5}
#' \item{PM10}
#' \item{Wind}
#' \item{Temperature}
#' \item{Barometric_Pressure}
#' \item{RH_and_Dewpoint}
#' \item{HAPs}
#' \item{VOCs}
#' \item{NONOxNOy}
#' }
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
#'
#' @return Tibble of EPA data.
#'
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
#' @references \href{https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_format_3}{file format description}
#'
#' @examples
#' \dontrun{
#' zipFile <- epa_downloadAQSData(2016, "88101", '~/Data/EPA')
#' tbl <- epa_parseAQSData(zipFile, "PM2.5")
#' }

epa_parseData <- function(
  zipFile = NULL
) {

  if ( logging.isInitialized() )
    logger.debug(" ----- epa_parseAQSData() ----- ")

  # Sanity checks
  if ( is.null(zipFile) ) {
    logger.error("Required parameter 'zipFile' is missing")
    stop(paste0("Required parameter 'zipFile' is missing"))
  }

  csvFile <- stringr::str_replace(zipFile,"\\.zip","\\.csv")

  # Uncompress
  if ( logging.isInitialized() )
    logger.trace(paste0('Uncompressing ',zipFile,' ...'))
  utils::unzip(zipFile, exdir=dirname(zipFile))
  if ( logging.isInitialized() )
    logger.trace(paste0('Finished uncompressing'))


  # Here are the column names from an EPA hourly dataset:

  #   [1] "State Code"          "County Code"         "Site Num"            "Parameter Code"      "POC"
  #   [6] "Latitude"            "Longitude"           "Datum"               "Parameter Name"      "Date Local"
  #   [11] "Time Local"          "Date GMT"            "Time GMT"            "Sample Measurement"  "Units of Measure"
  #   [16] "MDL"                 "Uncertainty"         "Qualifier"           "Method Type"         "Method Code"
  #   [21] "Method Name"         "State Name"          "County Name"         "Date of Last Change"

  # Assign appropriate data types
  col_types <- paste0("ccccc","ddccc","cccdc","ddccc","cccc")

  # Read in the data
  if ( logging.isInitialized() )
    logger.trace(paste0('Reading in ',csvFile,' ...'))
  tbl <- readr::read_csv(csvFile, col_types=col_types)
  if ( logging.isInitialized() )
    logger.trace(paste0('Finished reading in ',csvFile))

  # Cleanup
  file.remove(csvFile)

  if ( logging.isInitialized() )
    logger.trace('Downloaded and parsed %d rows of EPA data', nrow(tbl))

  return(tbl)
}
