#' @export
#' @import MazamaCoreUtils
#'
#' @title Download and parse tables of EPA AQS codes
#'
#' @description
#' Download and parse code tables from the US EPA.
#'
#' See https://www.epa.gov/aqs/aqs-code-list for a list of tables. the
#' \code{tableName} can be determined by hovering over each link. For example,
#' the \code{tableName} for the "General > Parameters" table is "parameters".
#'
#' @note Column types are determined automatically by \code{readr::read_csv()}
#' based on the first 1000 rows of input. In addition, any column matching
#' \code{" Code$"} will be converted to 'character' type (typically from
#' 'numeric'). It is up to the user to review column types
#' (with \code{dplyr::glimpse()}) and reassign where desired.
#'
#' @param tableName Character name associated with the code table.
#' @param baseUrl Character base URL for EPA AQS code tables.
#' @param quiet Logical passed on to \code{readr::read_csv(progress = !quiet)}.
#'
#' @return Tibble of EPA AQS codes.
#'
#' @references \href{https://www.epa.gov/aqs/aqs-code-list}{AQS Codes and Descriptions}
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
#' # Get a table of parameter codes
#' parameter_codes <- epa_aqs_getCodes(tableName = "parameters", quiet = FALSE)
#' }

epa_aqs_getCodes <- function(
  tableName = NULL,
  baseUrl = 'https://aqs.epa.gov/aqsweb/documents/codetables/',
  quiet = TRUE
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- epa_aqs_getCodes() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(tableName)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Parse csv file -------------------------------------------------------

  # e.g. https://aqs.epa.gov/aqsweb/documents/codetables/methods_all.csv
  csvFile <- paste0(baseUrl, tableName, ".csv")

  # Read in the data
  if ( logger.isInitialized() )
    logger.trace('Parsing %s ...', csvFile)

  result <- try({
    tbl <- readr::read_csv(csvFile, progress = !quiet)
  }, silent = quiet)

  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    if ( logger.isInitialized() )
      logger.error(err_msg)
    stop(err_msg)
  }

  # Convert any "~ Code" column to character
  if ( logger.isInitialized() )
    logger.trace('Converting Codes to character type ...')

  code_columns <- stringr::str_subset(names(tbl), " Code$")
  tbl <-
    tbl %>%
    dplyr::mutate_at(code_columns, as.character)

  if ( logger.isInitialized() )
    logger.trace('Finished parsing %s', csvFile)

  return(tbl)

}
