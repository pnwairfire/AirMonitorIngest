# AirMonitorIngest 0.2.0

Version 0.2 focuses on data ingest from AirNow using the AirNow API.

Initial functions include:

* `airnow_getSites()`
* `airnow_api_getData()`
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
