% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airnow_getData.R
\name{airnow_getData}
\alias{airnow_getData}
\title{Download and parse large amounts of data from the AirNow data API}
\usage{
airnow_getData(
  parameterName = c("PM2.5"),
  starttime = NULL,
  endtime = NULL,
  timezone = "UTC",
  monitorType = c("both", "permanent", "mobile"),
  instrumentType = c("both", "monitor", "sensor"),
  baseUrl = "https://www.airnowapi.org/aq/data/"
)
}
\arguments{
\item{parameterName}{One or more EPA AQS criteria parameter names.}

\item{starttime}{Desired start datetime (ISO 8601).}

\item{endtime}{Desired end datetime (ISO 8601).}

\item{timezone}{Olson timezone used to interpret dates (required).}

\item{monitorType}{Specifies mobile or permanent monitor deployments.}

\item{instrumentType}{Specifies regulatory monitors or low cost sensors.}

\item{baseUrl}{Base URL for archived hourly data.}
}
\value{
Tibble of AirNow hourly data.
}
\description{
This function uses the AirNow data webservice to retrieve
subsets of data that do not exceed a maximum data size which causes
errors.

Datetimes can be anything that is understood by
\code{MazamaCoreUtils::parseDatetime()}. For non-POSIXct values, the
recommended format is "YYYY-mm-dd HH:00:00" or just "YYYYmmddhh".
}
\examples{
\dontrun{
library(AirMonitorIngest)

setAPIKey("airnow", "<AIRNOW_API_KEY>")

tbl <-
  airnow_getData(
    parameterName = "PM2.5",
    starttime = 2021101200,
    endtime = 2021101300,
    timezone = "America/Los_Angeles",
    monitorType = "permanent"
   )

}
}
