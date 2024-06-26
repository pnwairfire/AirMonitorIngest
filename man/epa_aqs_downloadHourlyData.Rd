% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epa_aqs_downloadHourlyData.R
\name{epa_aqs_downloadHourlyData}
\alias{epa_aqs_downloadHourlyData}
\title{Download hourly EPA air quality data}
\usage{
epa_aqs_downloadHourlyData(
  parameterCode = "88101",
  year = NULL,
  baseUrl = "https://aqs.epa.gov/aqsweb/airdata/",
  downloadDir = tempdir(),
  quiet = TRUE
)
}
\arguments{
\item{parameterCode}{Character pollutant code.}

\item{year}{Ingeter year.}

\item{baseUrl}{Character base URL for the EPA AQS archive.}

\item{downloadDir}{Directory where .zip file will be saved.}

\item{quiet}{Logical passed on to \code{utils::download.file()}.}
}
\value{
Filepath of the downloaded zip file.
}
\description{
Download hourly air quality data from the US EPA and save it to a directory.

EPA parameter codes and (start year) include:

\describe{
\item{44201}{ -- Ozone (1980)}
\item{42401}{ -- SO2 (1980)}
\item{42101}{ -- CO (1980)}
\item{42602}{ -- NO2 (1980)}
\item{88101}{ -- PM2.5 FRM/FEM (2008)}
\item{88502}{ -- PM2.5 non FRM/FEM (1998)}
\item{81102}{ -- PM10 (1988)}
\item{SPEC}{ -- PM2.5 Speciation(2001)}
\item{PM10SPEC}{ -- PM10 Speciation (1988)}
\item{WIND}{ -- Winds (1980)}
\item{TEMP}{ -- Temperature (1980)}
\item{PRESS}{ -- Barometric Pressure (1980)}
\item{RH_DP}{ -- RH and Dewpoint (1980)}
\item{HAPS}{ -- HAPs (1993)}
\item{VOCS}{ -- VOCs (1980)}
\item{NONOxNOy}{ -- Nitrous Oxides (1980)}
}
}
\note{
Unzipped CSV files are almost 100X larger than the compressed .zip files.

Depending on your internet connection speeds, downloading files from the EPA
site can take several minutes per file. If you see warning messages that you have
timed out, please download files manually. Functions that process these files
will then discover the downloaded files in the \code{downloadDir}.
}
\examples{
\dontrun{
library(AirMonitorIngest)

# Create a directory specifically for EPA data
dir.create("~/Data/EPA", recursive = TRUE)

# Set logging level so messages and errors will appear in the console
MazamaCoreUtils::initializeLogging(logDir = "~/Data/EPA/")
logger.setLevel(TRACE)

# Save the download in ~/Data/EPA
zipFile <-
  epa_aqs_downloadHourlyData(
    parameterCode = "88101",
    year = 2008,
    downloadDir = "~/Data/EPA/",
    quiet = FALSE
  )

# Uncompress and parse into a tibble
tbl <- epa_aqs_parseHourlyData(zipFile)
}
}
\references{
\href{https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw}{EPA AirData Pre-Generated Data Files}
}
