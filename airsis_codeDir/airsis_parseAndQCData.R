#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Parse and QC raw AIRSIS data
#'
#' @param fileString Character string containing AIRSIS data.
#'
#' @description Raw character data from AIRSIS are parsed into a tibble.
#' The incoming \code{fileString} can be read in directly from AIRSIS using
#' \code{airsis_downloadData()} or from a local file using
#' \code{readr::read_file()}.
#'
#' @return Tibble of parsed and QC'ed monitor data.

airsis_parseAndQCData <- function(
  fileString = NULL
) {

  logger.debug(" ----- airsis_parseAndQCData() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(fileString)

  # ----- Identify data format -------------------------------------------------

  identify_function <- "airsis_identifyDataFormat"

  if ( !exists(identify_function) ) {
    msg <- sprintf("function '%s()' is not defined in %s", identify_function, identify_script)
    logger.error(msg)
    stop(msg)
  }

  # Identify data format
  dataFormat <- get(identify_function)(fileString)

  # ----- Parse monitor data ---------------------------------------------------

  logger.trace("Parsing data as: %s", dataFormat)

  parse_function <- sprintf("airsis_parse_%s", dataFormat)

  if ( !exists(parse_function) ) {
    msg <- sprintf("function '%s()' is not defined in %s", parse_function, parse_script)
    logger.error(msg)
    stop(msg)
  }

  tbl <- get(parse_function)(fileString)

  # ----- QC monitor data ------------------------------------------------------

  qc_function <- sprintf("airsis_QC_%s", dataFormat)

  if ( !exists(qc_function) ) {
    msg <- sprintf("function '%s()' is not defined in %s", qc_function, qc_script)
    logger.error(msg)
    stop(msg)
  }

  tbl <- get(qc_function)(tbl)

  # ----- Add metadata ---------------------------------------------------------

  tbl$airsis_dataFormat = dataFormat

  # ----- Return ---------------------------------------------------------------

  logger.trace("Returning %d rows of data", nrow(tbl))

  return(tbl)

}
