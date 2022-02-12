[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/AirMonitorIngest)](https://cran.r-project.org/package=AirMonitorIngest) [![Downloads](http://cranlogs.r-pkg.org/badges/AirMonitorIngest)](https://cran.r-project.org/package=AirMonitorIngest)
[![Build Status](https://travis-ci.org/MazamaScience/AirMonitorIngest.svg?branch=master)](https://travis-ci.org/MazamaScience/AirMonitorIngest)

# AirMonitorIngest

```
A suite of utility functions for ingesting hourly air quality monitoring data. 
Data from various sources are harmonized to use standardized variable names and 
a compact data model with spatial metadata stored in a 'meta' dataframe while 
hourly data are stored in a separate 'data' dataframe.
```

## Background

The USFS AirFire group is focused on air quality measurements associated with 
wildfire smoke and maintains both historical and real-time databases of PM2.5 
monitoring data obtained from stationary monitors. This data is used in 
operational displays and for retrospective analysis. Data ingest and management 
of air quality “stationary time series” are both important ongoing activities.

The **AirMonitorIngest** package is used to create data archives for the
**[AirMonitor](https://github.com/MazamaScience/AirMonitor)** 
package and isolates the work of meticulously cleaning,
validating and harmonizing data from various sources. In
our experience, every new data source or data format requires dedicated code to
bring it into a standardized system. Because new data sources appear every few
months, putting all of the data ingest code into a separate package allows 
downstream packages that work with harmonized data to be much more stable.

## Installation

Install from CRAN with:

`install.packages('AirMonitorIngest')`

Install the latest version from GitHub with:

`devtools::install_github('mazamascience/AirMonitorIngest')`

## Features

Although each source of data is unique, a similar set of functions is used to
obtain, parse and harmonize the data. The basic suite of functions associated 
with any source of data will include:

* `<source>_openWebPages()` -- Opens web pages relevant to the source data.
* `<source>_get~()` -- Gets tables of non-data information.
* `<source>_downloadData()` -- Download ASCII data.
* `<source>_parseData()` -- Parse ASCII data.
* `<source>_createData()` -- Create the `data` dataframe.
* `<source>_createMeta()` -- Create the `meta` dataframe.

------------------------------------------------------------------------

This project is supported by the [USFS AirFire](https://www.airfire.org) group.
