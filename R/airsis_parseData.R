#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Parse AIRSIS data string
#'
#' @param fileString Character string containing WRCC data.
#' @param codeDir Directory where AIRSIS data parsing scripts are found.
#'
#' @description Raw character data from AIRSIS are parsed into a tibble.
#' The incoming \code{fileString} can be read in directly from AIRSIS using
#' \code{airsis_downloadData()} or from a local file using
#' \code{readr::read_file()}.
#'
#' @details AIRSIS provides monitor data in an extremely raw fashion and does
#' minimal harmonizing of data. Every year, several new, slightly different data
#' formats become available. For this reason, all AIRSIS parsing code is
#' kept outside this package and must exist in \code{codeDir}. This allows for
#' quick addition of support for new formats without having to rebuild the
#' package and any systems dependent upon this package.
#'
#' @return Dataframe of AIRSIS raw monitor data.

airsis_parseData <- function(
  fileString = NULL,
  codeDir = NULL
) {

  logger.debug(" ----- airsis_parseData() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(fileString)
  MazamaCoreUtils::stopIfNull(codeDir)

  if ( !dir.exists(codeDir) )
    stop("cannot find 'codeDir': %s", codeDir)

  # ----- Identify data format -------------------------------------------------

  identify_function <- "airsis_identifyDataFormat"
  identify_script <- file.path(codeDir, sprintf("%s.R", identify_function))

  if ( !file.exists(identify_script) ) {
    msg <- sprintf("cannot find %s", identify_script)
    logger.error(msg)
    stop(msg)
  }

  source(identify_script)

  if ( !exists(identify_function) ) {
    msg <- sprintf("function '%s()' is not defined in %s", identify_function, identify_script)
    logger.error(msg)
    stop(msg)
  }

  # Identify data format
  dataFormat <- get(identify_function)(fileString)

  # ----- Parse monitor data ---------------------------------------------------

  parse_function <- sprintf("airsis_parse_%s", dataFormat)
  parse_script <- file.path(codeDir, sprintf("%s.R", parse_function))

  if ( !file.exists(parse_script) ) {
    msg <- sprintf("cannot find %s", parse_script)
    logger.error(msg)
    stop(msg)
  }

  source(parse_script)

  if ( !exists(parse_function) ) {
    msg <- sprintf("function '%s()' is not defined in %s", parse_function, parse_script)
    logger.error(msg)
    stop(msg)
  }


  tbl <- get(parse_function)(fileString)

  # ----- Return ---------------------------------------------------------------

  logger.trace("Returning %d rows of data", nrow(tbl))

  return(tbl)

}
