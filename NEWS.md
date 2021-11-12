# AirMonitorIngest 0.2.6

* Fixed a bug in `airnow_createMeta()`.
* Removed expectation in `airnow_createData()` that all monitors in `meta`
will be represted in the incoming data.

# AirMonitorIngest 0.2.5

* Removed `~APIKey()` functionality. Now importing this from 
`MazamaCoreUtils` 0.4.10.

# AirMonitorIngest 0.2.4

* Harmonized argument names and order in `epa_aqs_~()` and `airnow_~()` function 
signatures.

# AirMonitorIngest 0.2.3

Added functions to create AirNow 'data' and 'meta' objects:

* `airnow_update()`
* `airnow_updateKnownLocations()`
* `airnow_updateUnknownLocations()`
* `airnow_createMeta()`
* `airnow_createData()`

# AirMonitorIngest 0.2.2

* Renamed `airnow_api_~()` functions to just `airnow_~()`.
* Renamed `pollutant` to `parameterName` in `airnow_getData~()` for harmony with
`epa_aqs_~()` functions.

# AirMonitorIngest 0.2.1

* Updated `airnow_api_getDataSubset()` to request more variables.
* Removed `includeSiteMeta` argument from `airnow_api_getData~()`. Now we always get it.

# AirMonitorIngest 0.2.0

Version 0.2 focuses on data ingest from AirNow using the AirNow API.

Initial functions include:

* `airnow_getSites()`
* `airnow_api_getData()`
* `airnow_api_getDataSubset()`
* `airnow_openWebPages()`
* `get/setAPIKey()`

# AirMonitorIngest 0.1.3

* Support for EPA AQS 81102 -- PM10.

# AirMonitorIngest 0.1.1

* Importing **AirMonitor**.
* Updates to support `AirMonitor::coreMetadataNames`.
* Support for EPA AQS 88101 -- PM2.5 FRM/FEM.
* Support for EPA AQS 88402 -- PM2.5 non-FRM/FEM.
* Support for EPA AQS 42101 -- CO.
* Support for EPA AQS 44201 -- OZONE.

# AirMonitorIngest 0.1.0

Version 0.1 focuses on data ingest functions for EPA AQS data. Functions fall
into the following categories:

**Learn more about EPA AQS data**

* `epa_aqs_openWebPages()` 

**Get tables of information**

* `epa_aqs_getCodes()`
* `epa_aqs_getFormatDescriptionTables()`
* `epa_aqs_getMonitors()`
* `epa_aqs_getSites()`

**Harmonize EPA AQS measurements**

* `epa_aqs_createData()`
* `epa_aqs_createLocalArchive()`
* `epa_aqs_createMeta()`
* `epa_aqs_downloadHourlyData()`
* `epa_aqs_parseHourlyData()`

# AirMonitorIngest 0.0.1

* Initial setup.
