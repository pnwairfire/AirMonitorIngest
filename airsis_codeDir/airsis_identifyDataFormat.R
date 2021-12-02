#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Identify AIRSIS data format
#'
#' @param fileString Character string containing AIRSIS data.
#'
#' @description Examine column names in the first line of raw ASCII
#' data to identify different data formats provided by AIRSIS.
#'
#' Recognized AIRSIS formats include:
#'
#' \itemize{
#' \item{\code{"BAM 1020"}}
#' \item{\code{"eBam"}}
#' \item{\code{"eBam-New"}}
#' \item{\code{"EBAM Multi"}}
#' \item{\code{"EBAM Multi 2"}}
#' \item{\code{"EBAM Plus Multi"}}
#' \item{\code{"ESAM Multi"}}
#' \item{\code{"Iridium - Ebam"}}
#' \item{\code{"Iridium - Esam"}}
#' \item{\code{"Iridium"}}
#' }
#'

airsis_identifyDataFormat <- function(
  fileString = NULL
) {

  logger.debug(" ----- airsis_identifyMonitorType() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(fileString)

  if ( class(fileString)[1] != "character" ) {
    logger.error('AIRSIS fileString is of type %s', class(fileString)[1])
    stop(paste0('AIRSIS fileString is of type %s', class(fileString)[1]))
  }

  # ----- Different header styles ----------------------------------------------

  formatTypes <- list(

    # * apcd.airsis.com formats -----

    # provider = "APCD"; unitID = "1012"; year = 2013
    "MasterTable_ID,Alias,Latitude,Longitude,Conc (\u00b5g/m3),Qtot (m3),WS (KTS),Ozone (ppb),RTM09 (mg3),RH (%),Ambient Temp (C),TimeStamp,PDate" =
      "BAM 1020",

    # provider = "USFS"; unitID = "1078"; year = 2021
    "MasterTable_ID,Alias,Latitude,Longitude,COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Oceaneering Unit Voltage,Type,TimeStamp,PDate" =
      "EBAM Multi",

    # provider = "USFS"; unitID = "1085"; year = 2021
    "MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,ConcRT,ConcHR,ConcS(mg/m3),Flow,W/S,W/D,AT,RHx,BP(mmHg),RHi,Oceaneering Unit Voltage,FT,Status,Type,BV,TimeStamp,PDate" =
      "EBAM Multi 2",

    # provider = "APCD"; unitID = "1039"; year = 2021
    "MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,TimeStamp,PDate"  =
      "EBAM Plus Multi",

    # provider = "USFS"; unitID = "93"; year = 2008
    "MasterTable_ID,Alias,Latitude,Longitude,Serial Number,Version,Date/Time,ConcRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Data,TimeStamp,PDate" =
      "eBam-New",

    # provider = "USFS"; unitID = "1013"; year = 2020
    "MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,TimeStamp,PDate"   =
      "Iridium - Ebam",

    # provider = "USFS"; unitID = "1063"; year = 2021
    "MasterTable_ID,Alias,Latitude,Longitude,Conc(mg/m3),Flow(l/m),AT(C),BP(PA),RHx(%),RHi(%),WS(M/S),WD(Deg),BV(V),Alarm,Start Date/Time (GMT),Serial Number,System Volts,Data 1,Data 2,TimeStamp,PDate"    =
      "Iridium - Esam",

    # * arb2.airsis.com formats -----

    # No new formats

    # * arb3.airsis.com formats -----

    # No new formats

    # * epa.airsis.com formats -----

    #
    #  "EPA - EBAM",

    # provider = "EPA"; unitID = "1000"; year = 2019
    "MasterTable_ID,Alias,Latitude,Longitude,Date/Time/GMT,Start Date/Time (GMT),COncRT,ConcHr,Flow,W/S,W/D,AT,RHx,RHi,BV,FT,Alarm,Type,Serial Number,Version,Sys. Volts,TimeStamp,PDate" =
      "Iridium",

    # * mariposa.airsis.com formats -----

    # No new formats

    # * usfs.airsis.com formats -----

    #
    #  "AutoMet",

    #
    #  "BAM1020 Multi",

    #
    #  "Dataram",

    #
    #  "eBam",
    # provider = "USFS"; unitID = "70"; year = 2010

    # provider = "USFS"; unitID = "36"; year = 2007
    "MasterTable_ID,Alias,Latitude,Longitude,Time,DataCol1,eBam,TimeStamp,PDate" =
      "eBam",

    # provider = "USFS"; unitID = "1070"; year = 2021
    "MasterTable_ID,Alias,Latitude,Longitude,Conc(mg/m3),Flow(l/m),AT(C),BP(PA),RHx(%),RHi(%),WS(M/S),WD(Deg),BV(V),Alarm,Oceaneering Unit Voltage,TimeStamp,PDate"  =
      "ESAM Multi"

  )

  # ----- Determine format type ------------------------------------------------

  header <- readr::read_lines(fileString)[1]

  if ( !header %in% names(formatTypes) ) {
    msg <- sprintf("header not recognized:\n%s", header)
    logger.error(msg)
    stop(msg)
  }

  formatType <-
    formatTypes[header] %>%
    as.character() %>%
    make.names()

  # ----- Return ---------------------------------------------------------------

  return(formatType)

}
