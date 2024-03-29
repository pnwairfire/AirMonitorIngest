% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epa_aqs_createMeta.R
\name{epa_aqs_createMeta}
\alias{epa_aqs_createMeta}
\title{Create a 'meta' dataframe with required monitor metadata}
\usage{
epa_aqs_createMeta(
  AQS_monitors = NULL,
  sites_locationTbl = NULL,
  parameterCode = NULL
)
}
\arguments{
\item{AQS_monitors}{Table of monitor data obtained with \code{epa_aqs_getMonitors()}.}

\item{sites_locationTbl}{Table of "known locations" produced with \pkg{MazamaLocationUtils}.}

\item{parameterCode}{EPA "Parameter Code".}
}
\value{
Tibble of device-deployment metadata.
}
\description{
Create a \code{meta} dataframe with EPA AQS monitor metadata appropriate
for use with the \pkg{MazamaTimeSeries} package.

The data model is that monitor metadata are stored in a tibble named \code{meta}.
with a \code{deviceDeploymentID} unique identifier that is matched by column
names in an associated \code{data} file.
}
