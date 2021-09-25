# Creating a "known locations" table for AQS sites

library(AirMonitorIngest)

# Create a directory specifically for EPA data
dir.create("~/Data/EPA", showWarnings = FALSE, recursive = TRUE)

# Set logging level so messages and errors will appear in the console
logger.setLevel(TRACE)

# Get site metadata
AQS_sites <- epa_getAQSSites(downloadDir = "~/Data/EPA", quiet = FALSE)

# > dim(AQS_sites)
# [1] 20730    28
# > names(AQS_sites)
# [1] "State Code"            "County Code"           "Site Number"
# [4] "Latitude"              "Longitude"             "Datum"
# [7] "Elevation"             "Land Use"              "Location Setting"
# [10] "Site Established Date" "Site Closed Date"      "Met Site State Code"
# [13] "Met Site County Code"  "Met Site Site Number"  "Met Site Type"
# [16] "Met Site Distance"     "Met Site Direction"    "GMT Offset"
# [19] "Owning Agency"         "Local Site Name"       "Address"
# [22] "Zip Code"              "State Name"            "County Name"
# [25] "City Name"             "CBSA Name"             "Tribe Name"
# [28] "Extraction Date"

# Get monitor metadata
AQS_monitors <- epa_getAQSMonitors(downloadDir = "~/Data/EPA", quiet = FALSE)

# > dim(AQS_monitors)
# [1] 356777     32
# > names(AQS_monitors)
# [1] "State Code"                   "County Code"
# [3] "Site Number"                  "Parameter Code"
# [5] "Parameter Name"               "POC"
# [7] "Latitude"                     "Longitude"
# [9] "Datum"                        "First Year of Data"
# [11] "Last Sample Date"             "Monitor Type"
# [13] "Networks"                     "Reporting Agency"
# [15] "PQAO"                         "Collecting Agency"
# [17] "Exclusions"                   "Monitoring Objective"
# [19] "Last Method Code"             "Last Method"
# [21] "Measurement Scale"            "Measurement Scale Definition"
# [23] "NAAQS Primary Monitor"        "QA Primary Monitor"
# [25] "Local Site Name"              "Address"
# [27] "State Name"                   "County Name"
# [29] "City Name"                    "CBSA Name"
# [31] "Tribe Name"                   "Extraction Date"

# Get parameter codes
AQS_parameterCodes <- epa_getAQSCodes(tableName = "parameters", quiet = FALSE)

# > dim(AQS_parameterCodes)
# [1] 1477    8
# > names(AQS_parameterCodes)
# [1] "Parameter Code"           "Parameter"                "Parameter Abbreviation"
# [4] "Parameter Alternate Name" "CAS Number"               "Standard Units"
# [7] "Still Valid"              "Round or Truncate"

# ----- What monitoring scales do we have? -------------------------------------

AQS_monitors %>%
  dplyr::mutate(scale_definition = paste(`Measurement Scale`, `Measurement Scale Definition`, sep = ' -- ')) %>%
  dplyr::pull(`scale_definition`) %>%
  unique() %>% sort()

# [1] "MICROSCALE -- 0 M TO 100 M"
# [2] "MIDDLE SCALE -- 100 M TO 500 M"
# [3] "NA -- NA"
# [4] "NEIGHBORHOOD -- 500 M TO 4KM"
# [5] "REGIONAL SCALE -- 50 TO HUNDREDS KM"
# [6] "URBAN SCALE -- 4 KM TO 50 KM"

# * Parameter codes for MICROSCALE monitoring
AQS_monitors %>%
  dplyr::filter(`Measurement Scale` == "MICROSCALE") %>%
  dplyr::pull(`Parameter Name`) %>%
  unique() %>% sort()

# ==> Pretty much all of them so skip any idea of subsetting by Scale

# ----- Harmonize AQS_sites table ----------------------------------------------

# We want to make the AQS_sites table look as much like a "known locations"
# table as possible with the following columns:
#
# [1] "locationID"   "locationName" "longitude"    "latitude"
# [5] "elevation"    "countryCode"  "stateCode"    "county"
# [9] "timezone"     "houseNumber"  "street"       "city"
# [13] "zip"


harmonized_AQS_sites <-
  AQS_sites %>%

  # * Rename all existing columns with "AQS_" -----

  dplyr::rename_all(make.names) %>%
  dplyr::rename_all(~ gsub("^", "AQS_", .x)) %>%

  # * Add "known location" columns derived from AQS columns -----

  dplyr::mutate(
    locationID = as.character(NA),
    locationName = AQS_Local.Site.Name,
    longitude = AQS_Longitude,
    latitude = AQS_Latitude,
    elevation = AQS_Elevation,
    countryCode = as.character(NA),
    stateCode = MazamaSpatialUtils::US_stateFIPSToCode(AQS_State.Code),
    county = AQS_County.Name,
    timezone = as.character(NA),
    houseNumber = as.character(NA),
    street = as.character(NA),
    city = AQS_City.Name,
    zip = AQS_Zip.Code
  )

# * Add countryCode -----

harmonized_AQS_sites$countryCode <-
  dplyr::case_when(
    harmonized_AQS_sites$AQS_State.Code == "80" ~ "MX",
    harmonized_AQS_sites$AQS_State.Code == "CC" ~ "CA",
    TRUE ~ "US" # default
  )

# * Reorganize columns -----

known_location_names <- c(
  "locationID", "locationName", "longitude", "latitude",
  "elevation", "countryCode", "stateCode", "county",
  "timezone", "houseNumber", "street", "city", "zip"
)

AQS_names <-
  names(harmonized_AQS_sites) %>%
  stringr::str_subset("AQS_.*")

harmonized_AQS_sites <-
  harmonized_AQS_sites %>%
  dplyr::select(c(known_location_names, AQS_names))

# * Remove long/lat = 0 -----

# TODO

# * Other -----

# TODO

# ----- Create a "known location" table ----------------------------------------

library(MazamaLocationUtils)

# Initialize with standard directories
MazamaLocationUtils::mazama_initialize()
MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")


AirFire_EPA_AQS_500m <-
  MazamaLocationUtils::table_initializeExisting(
    harmonized_AQS_sites,
    stateDataset = "NaturalEarthAdm1",
    countryCodes = c("CA", "US", "MX"),
    radius = 500,
    verbose = TRUE
  )




