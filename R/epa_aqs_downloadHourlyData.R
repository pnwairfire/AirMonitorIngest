#' @export
#' @import MazamaCoreUtils
#'
#' @title Download hourly EPA air quality data
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
#' @note
#' Depending on your internet connection speeds, downloading files from the EPA
#' site can take several minutes per file. If you see warning messages that you have
#' timed out, please download files manually. Functions that process these files
#' will then discover the downloaded files in the \code{downloadDir}.

#' @param year Ingeter year.
#' @param parameterCode Character pollutant code.
#' @param downloadDir Directory where .zip file will be saved.
#' @param baseUrl Character base URL for the EPA AQS archive.
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
#' # Save the download in ~/Data/EPA
#' zipFile <- epa_aqs_downloadHourlyData(2008, "88101", "~/Data/EPA/", quiet = FALSE)
#'
#' # Uncompress and parse into a tibble
#' tbl <- epa_aqs_parseHourlyData(zipFile)
#' }

epa_aqs_downloadHourlyData <- function(
  year = NULL,
  parameterCode = "88101",
  downloadDir = tempdir(),
  baseUrl = 'https://aqs.epa.gov/aqsweb/airdata/',
  quiet = TRUE
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- epa_aqs_downloadHourlyData() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(year)
  MazamaCoreUtils::stopIfNull(parameterCode)
  MazamaCoreUtils::stopIfNull(downloadDir)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # * Parameter code -----
  validParameterCodes <- c(
    "44201", "42401", "42101", "42602", "88101", "88502", "81102",
    "SPEC", "WIND", "TEMP", "PRESS", "RH_DP", "HAPS", "VOCS", "NONOxNOy"
  )

  parameterCode <- as.character(parameterCode)
  if ( !parameterCode %in% validParameterCodes ) {
    err_msg <- sprintf(
      "parameterCode '%s' is not in: %s",
      parameterCode, paste0(validParameterCodes, collapse = ", ")
    )
    if ( logger.isInitialized() ) logger.error(err_msg)
    stop(err_msg)
  }

  # * Year -----
  if ( year < 1980 ) {
    err_msg <- "No data available before 1980"
    if ( logger.isInitialized() ) logger.error(err_msg)
    stop(err_msg)
  } else if ( (parameterCode == "88101"    && year < 2008) ||
              (parameterCode == "88502"    && year < 1998) ||
              (parameterCode == "81102"    && year < 1988) ||
              (parameterCode == "SPEC"     && year < 2001) ||
              (parameterCode == "PM10SPEC" && year < 1998) ||
              (parameterCode == "HAPS"     && year < 1993) ) {
    err_msg <- sprintf("No data available for parameter code %s in year %i", parameterCode, year)
    if ( logger.isInitialized() ) logger.error(err_msg)
    stop(err_msg)
  }

  # ----- Download data --------------------------------------------------------

  # Set up file names and paths
  fileBase <- paste("hourly", parameterCode, year, sep = "_")
  url <- paste0(baseUrl, fileBase, ".zip")
  zipFile <- path.expand( paste0(file.path(downloadDir, fileBase), ".zip") )

  if ( !file.exists(zipFile) ) {

    if ( logger.isInitialized() )
      logger.trace('Downloading %s.zip ...', fileBase)

    result <- try({
      utils::download.file(url, zipFile, quiet = quiet)
    }, silent = quiet)

    if ( "try-error" %in% class(result) ) {
      base::unlink(zipFile) # cleanup
      err_msg <- geterrmessage()
      if ( logger.isInitialized() )
        logger.error(err_msg)
      stop(err_msg)
    }

    if ( logger.isInitialized() )
      logger.trace(paste0('Finished downloading.'))

  } else {

    if ( logger.isInitialized() )
      logger.trace('Found %s.zip in %s', fileBase, downloadDir)

  }

  return(zipFile)

}
