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
    "datetime", "longitude", "latitude",
    "flow", "AT", "RHi", "pm25", "voltage"
  )

  # Times -- "1/1/2010 12:00:00 AM"
  datetime <-
    lubridate::mdy_hms(tbl$TimeStamp, tz = "UTC") %>%
    lubridate::floor_date(datetime, unit = "hour")

  pm25 <- tbl[["Conc..\u00b5g.m3."]]

  # NOTE:  Can't use the pm25 column nameinside of dplyr becuase of:
  # NOTE:    "Error: \uxxxx sequences not supported inside backticks"

  tbl <-

    # Start with the tbl
    tbl %>%

    # Core variables
    dplyr::mutate(
      gpsRecord = !is.na(.data$Longitude),
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
    dplyr::select(dplyr::all_of(columnNames))



  # TODO:  deal with duplicated datetime

  # TODO:  Conert flow to standard units

  # # ----- Various fixes --------------------------------------------------------
  #
  # # Check to see if any records remain
  # if ( nrow(tbl) == 0 ) {
  #   logger.error("No data remaining after parsing cleanup")
  #   stop("No data remaining after parsing cleanup", call. = FALSE)
  # }
  #
  # # NOTE:  Latitude, Longitude and Sys..Volts are measured at 6am and 6pm
  # # NOTE:  as separate GPS entries in the tibble. They need to be carried
  # # NOTE:  forward so they appear in all rows.
  #
  # gpsMask <- !is.na(tbl$Longitude)
  #
  # if (monitorType == "EBAM") {
  #   if ( monitorSubtype == "MULTI2_B") {
  #     voltLabel <- "Oceaneering.Unit.Voltage"
  #   } else {
  #     voltLabel <- "Sys..Volts"
  #   }
  # } else if (monitorType == "ESAM") {
  #   if ( monitorSubtype == "MULTI" ) {
  #     voltLabel <- "Oceaneering.Unit.Voltage"
  #   } else {
  #     voltLabel <- "System.Volts"
  #   }
  # } else {
  #   # NOTE: BAM1020s don't have voltage data
  #   voltLabel <- NULL
  # }
  #
  # # use "quosures" to let us use a variable as a column name
  # voltColumn <- rlang::enquo(voltLabel)
  #
  # # Propagate data forwards, then backwards to fill in missing values
  # tbl <- tbl %>%
  #   tidyr::fill(.data$Longitude, .data$Latitude, !!voltColumn) %>%
  #   tidyr::fill(.data$Longitude, .data$Latitude, !!voltColumn, .direction = "up")
  #
  # logger.trace("Removing %d 'GPS' records from raw data", sum(gpsMask))
  # tbl <- tbl[!gpsMask,]
  #
  # # ----- Return ---------------------------------------------------------------
  #
  # logger.trace('Retaining %d rows of raw %s measurements', nrow(tbl), monitorType)
  #
  # return(tbl)

}
