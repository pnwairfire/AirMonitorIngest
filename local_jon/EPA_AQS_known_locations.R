# Creating a "known locations" table for AQS sites

library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(AirMonitorIngest)

# Create a directory specifically for EPA data
dir.create("~/Data/EPA", showWarnings = FALSE, recursive = TRUE)

# Set logging level so messages and errors will appear in the console
logger.setLevel(TRACE)

# ----- Download data ----------------------------------------------------------

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

# ----- Create harmonized_AQS_sites table --------------------------------------

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
  dplyr::select(dplyr::all_of(c(known_location_names, AQS_names)))

# plot(harmonized_AQS_sites$longitude, harmonized_AQS_sites$latitude, pch = 15)

# ==> Need to remove lon/lat == 0

# * Remove lon/lat == 0 -----

harmonized_AQS_sites <-
  harmonized_AQS_sites %>%
  dplyr::filter(longitude != 0 & latitude != 0)

# ----- Add countryCode --------------------------------------------------------

harmonized_AQS_sites$countryCode <-
  dplyr::case_when(
    harmonized_AQS_sites$AQS_State.Code == "66" ~ "GU",
    harmonized_AQS_sites$AQS_State.Code == "78" ~ "VI",
    harmonized_AQS_sites$AQS_State.Code == "80" ~ "MX",
    harmonized_AQS_sites$AQS_State.Code == "CC" ~ "CA",
    TRUE ~ "US" # default
  )

# > any(is.na(harmonized_AQS_sites$countryCode))
# [1] FALSE
# > table(harmonized_AQS_sites$countryCode)
#
# CA    GU    MX    US    VI
# 4    25    41 20633    27

# ==> Looks good.

# ----- Add locationID ---------------------------------------------------------

# NOTE:  Add a unique identifier at this point so we can dplyr::join() later on.

harmonized_AQS_sites$locationID <-
  MazamaLocationUtils::location_createID(
    harmonized_AQS_sites$longitude,
    harmonized_AQS_sites$latitude
  )

# ----- Add timezones ----------------------------------------------------------

MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
MazamaSpatialUtils::loadSpatialData("Worldimezones")

# NOTE:  Taking some care with this as it takes a very long time

# NOTE:  EPA has monitors from US, Canada, Mexico, Puerto Rico, Virgin Islands and Guam

lon <- harmonized_AQS_sites$longitude
lat <- harmonized_AQS_sites$latitude

timezone <-
  MazamaSpatialUtils::getTimezone(
    lon,
    lat,
    dataset = "WorldTimezones",
    countryCodes = c("US", "CA", "MX", "PR", "VI", "GU"),
    useBuffering = TRUE
)

# Where do we still have missing values?
# > sum(is.na(timezone))
# [1] 1

# ==> Only one left. Figure out where it is.

# Add timezones
harmonized_AQS_sites$timezone <- timezone

# * Fix locations with missing timezones -----

# na_timezone <- dplyr::filter(harmonized_AQS_sites, is.na(timezone))
# View(na_timezone)

# Alachua, Florida is in the "America/New_York" timezone.

na_index <- which(is.na(harmonized_AQS_sites$timezone))
harmonized_AQS_sites$timezone[na_index] <- "America/New_York"

# > any(is.na(harmonized_AQS_sites$timezone))
# [1] FALSE
#
# ==> YAY!

# Now check the other key parameters

# > any(is.na(harmonized_AQS_sites$timezone))
# [1] FALSE
# > any(is.na(harmonized_AQS_sites$longitude))
# [1] FALSE
# > any(is.na(harmonized_AQS_sites$latitude))
# [1] FALSE
# > any(is.na(harmonized_AQS_sites$timezone))
# [1] FALSE
# > any(is.na(harmonized_AQS_sites$countryCode))
# [1] FALSE
# > any(is.na(harmonized_AQS_sites$stateCode))
# [1] TRUE

# ----- Fix stateCodes ---------------------------------------------------------

MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")

# Break tbl into two parts: with/out stateCode

has_stateCode <- dplyr::filter(harmonized_AQS_sites, !is.na(stateCode))
na_stateCode <- dplyr::filter(harmonized_AQS_sites, is.na(stateCode))

# View(na_stateCode)

# ==> We are missing stateCodes for "CA", "MX", "VI" and "GU"

# Add missing stateCodes
lon <- na_stateCode$longitude
lat <- na_stateCode$latitude

na_stateCode$stateCode <-
  MazamaSpatialUtils::getStateCode(
    lon,
    lat,
    dataset = "NaturalEarthAdm1",
    countryCodes = c("CA", "MX", "VI", "GU"),
    useBuffering = TRUE
  )

# Combine two tbls
harmonized_AQS_sites <-
  dplyr::bind_rows(
    has_stateCode,
    na_stateCode
  )

# dplyr::filter(harmonized_AQS_sites, is.na(stateCode)) %>% View()

# ==> Only missing for Guam which is OK!

# ----- Investigate duplicates -------------------------------------------------

# > sum(duplicated(harmonized_AQS_sites$locationID))
# [1] 368

# ==> Yikes! 368 duplicate locations

duplicate_indices <- which(duplicated(harmonized_AQS_sites$locationID))
duplicate_IDs <- harmonized_AQS_sites$locationID[duplicate_indices]

duplicates <-
  harmonized_AQS_sites %>%
  dplyr::filter(locationID %in% duplicate_IDs)

# View(duplicates)

# ==> Many of these have AQS_Site.Closed.Date < 1980

siteClosedYear <-
  stringr::str_sub(duplicates$AQS_Site.Closed.Date, 1, 4) %>%
  as.numeric()

# > sum(siteClosedYear >= 1980, na.rm = TRUE)
# [1] 271

fewer_duplicates <-
  duplicates %>%
  dplyr::filter(siteClosedYear >= 1980, na.rm = TRUE)

duplicate_indices <- which(duplicated(fewer_duplicates$locationID))
duplicate_IDs <- fewer_duplicates$locationID[duplicate_indices]

duplicates <-
  fewer_duplicates %>%
  dplyr::filter(locationID %in% duplicate_IDs)

# > dim(duplicates)
# [1] 209  41
# View(duplicates)

# ==> Ugh, but most have AQS_Site.Closed.Date < 2000

siteClosedYear <-
  stringr::str_sub(duplicates$AQS_Site.Closed.Date, 1, 4) %>%
  as.numeric()

# > sum(siteClosedYear >= 2000, na.rm = TRUE)
# [1] 33

fewer_duplicates <-
  duplicates %>%
  dplyr::filter(siteClosedYear >= 2000, na.rm = TRUE)

duplicate_indices <- which(duplicated(fewer_duplicates$locationID))
duplicate_IDs <- fewer_duplicates$locationID[duplicate_indices]

duplicates <-
  fewer_duplicates %>%
  dplyr::filter(locationID %in% duplicate_IDs)

# > dim(duplicates)
# [1] 10 41
# > View(duplicates)

# With this few, we will just remove the duplicates

# ----- Remove duplicates ------------------------------------------------------

# Break tbl into two parts: duplicated or not

duplicate_indices <- which(duplicated(harmonized_AQS_sites$locationID))
duplicate_IDs <- harmonized_AQS_sites$locationID[duplicate_indices]

duplicates <-
  harmonized_AQS_sites %>%
  dplyr::filter(locationID %in% duplicate_IDs)

no_duplicates <-
  harmonized_AQS_sites %>%
  dplyr::filter(! locationID %in% duplicate_IDs)

# * Remove duplicates with Site.Closed < 2000 -----

siteClosedYear <-
  stringr::str_sub(duplicates$AQS_Site.Closed.Date, 1, 4) %>%
  as.numeric()

mask <- is.na(siteClosedYear) | siteClosedYear >= 2000

duplicates <- duplicates[mask,]

duplicate_indices <- which(duplicated(duplicates$locationID))
duplicate_IDs <- duplicates$locationID[duplicate_indices]

# > length(duplicate_IDs)
# [1] 53

# * Just use dplyr::distinct() on the final 53 -----

duplicates <-
  duplicates %>%
  dplyr::distinct(locationID, .keep_all = TRUE)

# Combine two tbls
harmonized_AQS_sites <-
  dplyr::bind_rows(
    no_duplicates,
    duplicates
  )

# > dim(harmonized_AQS_sites)
# [1] 19285    41
# > any(duplicated(harmonized_AQS_sites$locationID))
# [1] FALSE

# ----- Check for locations that are too close ---------------------------------

radius <- 500    # meters

x <-
  harmonized_AQS_sites %>%
  dplyr::select(longitude, latitude)

# Calculate distances between each location
distances <- geodist::geodist(x, measure = "cheap") # or "geodesic"

# Get distances that are less than the given diameter
# NOTE: the distance between a location and itself is always zero
distancesLessThanR <- (distances != 0) & (distances < radius )

# Select the locations that are "too close".
tooClose <- which(distancesLessThanR > 0, arr.ind = TRUE)

if ( nrow(tooClose) > 0 ) {

  # NOTE:  If location a and b are too close, two entries will be returned:
  # NOTE:        row  col
  # NOTE:   [#,]  a    b
  # NOTE:    ...
  # NOTE:   [#,]  b    a
  #
  # NOTE:  While often the case, there is no guarantee that complementary
  # NOTE:  rows will be adjacent to eachother. The next couple of lines
  # NOTE:  find the rows that have the same indices and reduce the table to
  # NOTE:  only unique pairs.

  sortedMatrix <- t(apply(tooClose, 1, sort))
  tooClose <- sortedMatrix[!duplicated(sortedMatrix),]

  tooCloseCount <- nrow(tooClose)

  # Format the first line of the warning message
  firstLine <- sprintf(
    "%d locations have neighbors that are < %d m apart\n",
    round(tooCloseCount),
    diameter
  )

  # Create a vector of lines, on for each tooClose location pair
  tooCloseLines <- vector("character", length = tooCloseCount)
  for ( i in seq_len(nrow(tooClose)) ) {

    dist <- distances[tooClose[i, 1], tooClose[i, 2]]
    tooCloseLines[i] <- sprintf(
      "Distance: %6.1f -- rows %s %s",
      round(dist, 1),
      tooClose[i, 1],
      tooClose[i, 2]
    )

  }

  instructions <- "
The presence of locations closer than twice the specified radius invalidate the
uniqueness of a 'known locations' table and should be rectified. There are two
basic options:
  1) Reduce the radius to less than the minimum distance.
  2) Manually merge nearby locations to share the same longitude, latitude and
     locationID

Please review the returned locationTbl for the identified rows.
  "

  lines <- c(firstLine, tooCloseLines, instructions)

  # Paste the lines together
  warning(paste(lines, collapse = "\n"))

}



