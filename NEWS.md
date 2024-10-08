# AirMonitorIngest 0.5.0

Updated dependences:

- `AirMonitor` 0.3.1 => 0.4.1
- `MazamaCoreUtils` 0.4.13 => 0.5.2
- `MazamaLocationUtils` 0.3.7 => 0.4.4

- Removed internal `clusterByDistance()` in favor of
`MazamaLocationUtils::clusterByDistance()`.

# AirMonitorIngest 0.4.8

Added support for AirNow `instrumentType` parameter. This allows us to specify
whether we want include "sensors" as part of a data request. This parameter
can now be specified in:

- `airnow_getData()`
- `airnow_getDataCustom()`
- `airnow_getData()`

Default behavior is to include sensors in the data request which is the same
behavior as in all previous versions of the package.

# AirMonitorIngest 0.4.7

Moved the following functions to the **monitoring-data-ingest-v2** code base.

* `wrcc_downloadData()`
* `wrcc_getCurrentUnitIDs()`

# AirMonitorIngest 0.4.6

Moved the following functions to the **monitoring-data-ingest-v2** code base.

* `airsis_downloadData()`
* `airsis_getCurrentUnitIDs()`

# AirMonitorIngest 0.4.5

* Added `skip = 1` to `readr::read_delim()` to skip the header line when 
ingesting AirNow metadata.
* Removed unused dependency on **MazamaSpatialUtils**.

# AirMonitorIngest 0.4.4

* Refactored `addClustering()` into `addClustering()` and `clusterByDistance()` 
and added additional adjacency checks to improve results when an entire year's 
worth of data is used.

# AirMonitorIngest 0.4.3

Moved the following functions to the **monitoring-data-ingest-v2** code base.
These functions are changing more often than anticipated.

* `wrcc_EBAMQualityControl()`
* `wrcc_ESAMQualityControl()`
* `wrcc_createMetaAndData()`
* `wrcc_identifyDataFormat()`
* `wrcc_parseData()`
* `wrcc_qualityControl()`
* `wrcc_updateKnownLocations()`

# AirMonitorIngest 0.4.2

* Removed `airnow_updateKnownLocations()`, `airnow_updateUnknownLocations()`,
`airnow_update()`, `airnow_createData()` and `airnow_createMeta()`. These
functions are changing more often than anticipated and have been moved to the
**monitoring-data-ingest-v2** code base.

# AirMonitorIngest 0.4.1

* Guarantee that `clusterLon` and `clusterLat` returned from `addClustering()`
are pure numerics without any `names`.
* Rename `wrcc_identifyMonitorType()` to `wrcc_identifyDataFormat()`.
* Increased default `distanceThreshold` to 500m in `airnow_updateKnownLocations()`.

# AirMonitorIngest 0.4.0

Version 0.4 uses a new sites metadata file from AirNow, 
Monitoring_Site_Locations_v2.data, which has a different
set of fields. This data can be merged with the old file but requires some 
changes in downstream data processing scripts.

* Changes to `airnow_getSites()`, `airnow_updateKnownLocations()` and
`airnow_updateUnknownLocations()` to accommodate the new metadata.

# AirMonitorIngest 0.3.13

* Corrected column headers in `wrcc_identifyMonitorType()`.
* Completed "Working with WRCC data" article.

# AirMonitorIngest 0.3.12

* Add first vignettes and an article for Amber Ortega.

# AirMonitorIngest 0.3.11

* Final fix for `~_updateKnownLocations()`

# AirMonitorIngest 0.3.10

* Ensure distinct records when updating known location tables in
`airsis_updateKnownLocations()` and `wrcc_updateKnownLocations()`.

# AirMonitorIngest 0.3.9

* Added `wrcc_getUnitIDs()`. Removed `wrcc_getCurrentUnitIDs()`.

# AirMonitorIngest 0.3.8

* Added `airnow_getDataCustom()`.

# AirMonitorIngest 0.3.7

* Added `airnow_getHourlyData()`.

# AirMonitorIngest 0.3.6

* Renamed `wrcc_getDailyUnitIDs()` to `wrcc_getCurrrentUnitIDs()` and added 
explicit date information.
* Fixed use of `MazamaCoreUtils::setIfNull()`.

# AirMonitorIngest 0.3.5

* Fixed bug with duplicated `meta$deviceDeploymentID` when two clusters were
< 1000 meters apart.

# AirMonitorIngest 0.3.3

Adding `airsis_~()` functions:

* `airsis_getCurrentUnitIDs()`
* `airsis_downloadData()`
* `airsis_parseAndQCData()`
* `airsis_codeDir/airsis_identifyDataFormat()`
* `airsis_codeDir/parse_BAM.1020()`
* `airsis_codeDir/airsis_QC_BAM.1020()`
* `airsis_codeDir/airsis_updateKnownLocations()`

# AirMonitorIngest 0.3.2

* Now depending on **AirMonitor** 0.0.6.

# AirMonitorIngest 0.3.1

* Added `wrcc_getDailyUnitIDs()`.

# AirMonitorIngest 0.3.0

Version 0.3 focuses on data ingest from WRCC and AIRSIS.

Initial functions include:

* `wrcc_downloadData()`
* `wrcc_identifyMonitorType()`
* `wrcc_parseData()`
* `wrcc_qualityControl()`
* `wrcc_EBAMQualityControl()`
* `wrcc_ESAMQualityControl()`
* `wrcc_updateKnownLocations()`
* `wrcc_createMetaAndData()`
* `addClustering()`

# AirMonitorIngest 0.2.7

Updates for **AirMonitor** 0.0.4.

* Fixed a bug in `airnow_createMeta()` that occurs only when new locations
appear in the incoming raw data.
* Now exporting `%>%`.

# AirMonitorIngest 0.2.6

* Fixed a bug in `airnow_createMeta()`.
* Fixed a bug in `airnow_createData()` that kept records with no data.
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
