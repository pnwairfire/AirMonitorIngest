#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Parse AIRSIS "BAM 1020" formatted data
#'
#' @param fileString Character string containing AIRSIS data.
#'
#' @description Raw character data from AIRSIS are parsed into a tibble. The
#' incoming \code{fileString} can be read in directly from AIRSIS using
#' \code{airsis_downloadData()} or from a local file using \code{readr::read_file()}.
#'
#' The type of data format represented by \code{fileString} is inferred from the
#' column names using \code{airsis_identifyDataFormat()}. In this function, the
#' character data are then read into a tibble and augmented in the following ways:
#'
#' \enumerate{
#' \item{Longitude, Latitude and any System Voltage values, which are only
#' present in GPS timestamp rows, are propagated forwards using a
#' last-observation-carry-forward algorithm}
#' \item{Longitude, Latitude and any System Voltage values, which are only
#' present in GPS timestamp rows, are propagated backwards using a
#' first-observation-carry-backward algorithm}
#' \item{GPS timestamp rows are removed'}
#' }
#'
#' @return Tibble of raw AIRSIS monitor data.
#'

airsis_parse_BAM.1020 <- function(
  fileString = NULL
) {

  logger.debug(" ----- airsis_parse_BAM.1020() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(fileString)

  lines <- readr::read_lines(fileString)

  # ----- Parse the file -------------------------------------------------------

  # Convert the fileString into individual lines
  lines <- readr::read_lines(fileString)

  if ( length(lines) == 1 ) {
    msg <- "no valid data"
    logger.warn(msg)
    stop(msg)
  }

  # "MasterTable_ID,Alias,Latitude,Longitude,Conc (\u00b5g/m3),Qtot (m3),WS (KTS),Ozone (ppb),RTM09 (mg3),RH (%),Ambient Temp (C),TimeStamp,PDate"

  # Set up column types and names
  rawNames <- unlist(stringr::str_split(lines[1], ','))
  col_names <- make.names(rawNames)
  col_types <- 'ccdddddddddcc'

  # NOTE:  We need to guarantee that fakeFile always ends with a newline so that
  # NOTE:  read_lines() will interpret fakeFile as a single data record as
  # NOTE:  literal data and not a path.

  # Remove header line, leaving only data
  fakeFile <- paste0(paste0(lines[-1], collapse = '\n'),'\n')

  tbl <- suppressWarnings({
    readr::read_csv(fakeFile, col_names = col_names, col_types = col_types)
  })

  # Print out any problems encountered by readr::read_csv
  problemsDF <- readr::problems(tbl)
  if ( dim(problemsDF)[1] > 0 ) {
    logger.trace("Records skipped with parsing errors:")
    problems <- utils::capture.output(format(problemsDF))
    for (i in seq_along(problems)) {
      logger.trace("%s",problems[i])
    }
  }

  # ----- Format specific cleanup ----------------------------------------------

  # TODO:  provider = "USFS"; unitID = "49"; year = 2010
  # TODO:  This file had every other row missing; also, no lat/lon info.
  # TODO:  May want to look into this further if noticed in more recent data.


  # ----- Harmonize column names -----------------------------------------------

  # Core columns we keep
  columnNames <- c(
    "locationName",
    "datetime", "longitude", "latitude",
    "flow", "AT", "RHi", "pm25", "voltage"
  )

  # NOTE:  Assume that the TimeStamp is the time at which the just completed
  # NOTE:  hourly average is reported. So the average for the 08:00 hour will
  # NOTE:  complete at 09:00 but not be reported until, say, 09:14:49.
  # NOTE:  This is why we have to subtract one hour to get the correct
  # NOTE:  beginning-of-the-hour datetime value.

  # Times -- "5/22/2013 9:14:49 PM"
  datetime <-
    lubridate::mdy_hms(tbl$TimeStamp, tz = "UTC") %>%
    lubridate::floor_date(datetime, unit = "hour") - lubridate::dhours(1)

  pm25 <- tbl[["Conc..\u00b5g.m3."]]

  # NOTE:  Can't use the pm25 column name inside of dplyr becuase of:
  # NOTE:    "Error: \uxxxx sequences not supported inside backticks"

  tbl <-

    # Start with the tbl
    tbl %>%

    # Core variables
    dplyr::mutate(
      gpsRecord = !is.na(.data$Longitude),
      locationName = .data$Alias,
      datetime = !!datetime,
      longitude = .data$Longitude,
      latitude = .data$Latitude,
      flow = .data$Qtot..m3.,
      AT = .data$Ambient.Temp..C.,
      RHi = .data$RH....,
      pm25 = !!pm25,
      voltage = as.numeric(NA)
    ) %>%

    # Copy information from and then remove GPS records
    tidyr::fill(.data$longitude, .data$latitude, .direction = "down") %>%
    tidyr::fill(.data$longitude, .data$latitude, .direction = "up") %>%
    dplyr::filter(.data$gpsRecord == FALSE) %>%

    # Only keep core harmonized data
    dplyr::select(dplyr::all_of(columnNames)) %>%

    # Remove duplicate times using later-is-better logic
    dplyr::arrange(dplyr::desc(.data$datetime)) %>%
    dplyr::distinct(.data$datetime, .keep_all = TRUE) %>%
    dplyr::arrange(.data$datetime)

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(MazamaCoreUtils)

  startdate = MazamaCoreUtils::parseDatetime("2013-05-20", timezone = "UTC")
  enddate = MazamaCoreUtils::parseDatetime("2013-05-30", timezone = "UTC")
  timezone = "UTC"
  provider = "APCD"
  unitID = "1012"

  # Read in AIRSIS .csv data
  fileString <- airsis_downloadData(
    startdate,
    enddate,
    timezone,
    provider,
    unitID
  )

  dataFormat <- airsis_identifyDataFormat(fileString)

  tbl <- airsis_parse_BAM.1020(fileString)



}
