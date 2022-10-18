# Create a known locations table for EPA AQS

library(MazamaCoreUtils)
library(AirMonitorIngest)

# Create a directory specifically for EPA data
dir.create("~/Data/EPA", recursive = TRUE)

# Set logging level so messages and errors will appear in the console
MazamaCoreUtils::initializeLogging(logDir = "~/Data/EPA/")
logger.setLevel(TRACE)

# Download and parse site metadata
aqs_sites <- epa_aqs_getSites(downloadDir = "~/Data/EPA", quiet = FALSE)

# > dplyr::glimpse(aqs_sites, width = 75)
# Rows: 20,730
# Columns: 28
# $ `State Code`            <chr> "01", "01", "01", "01", "01", "01", "01",…
# $ `County Code`           <chr> "001", "001", "001", "003", "003", "003",…
# $ `Site Number`           <chr> "0001", "0002", "0003", "0001", "0002", "…
# $ Latitude                <dbl> 32.43746, 32.42847, 32.33266, 0.00000, 30…
# $ Longitude               <dbl> -86.47289, -86.44358, -86.79152, 0.00000,…
# $ Datum                   <chr> "WGS84", "WGS84", "WGS84", "NAD27", "WGS8…
# $ Elevation               <dbl> 64.00, 0.00, 41.00, 0.00, 0.00, 49.00, 37…
# $ `Land Use`              <chr> "RESIDENTIAL", "AGRICULTURAL", "FOREST", …
# $ `Location Setting`      <chr> "SUBURBAN", "RURAL", "RURAL", "RURAL", "R…
# $ `Site Established Date` <chr> "1974-05-01", "1980-01-01", "1989-08-31",…
# $ `Site Closed Date`      <chr> "1976-12-31", "1982-12-31", "1990-11-30",…
# $ `Met Site State Code`   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ `Met Site County Code`  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ `Met Site Site Number`  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ `Met Site Type`         <chr> NA, NA, NA, NA, NA, NA, "ON-SITE MET EQUI…
# $ `Met Site Distance`     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ `Met Site Direction`    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ `GMT Offset`            <dbl> -6, -6, -6, -6, -6, -6, -6, -6, -6, -6, -…
# $ `Owning Agency`         <chr> "Al Dept Of Env Mgt", "Al Dept Of Env Mgt…
# $ `Local Site Name`       <chr> NA, NA, NA, NA, NA, NA, "FAIRHOPE, Alabam…
# $ Address                 <chr> "KING ARTHUR TRAILER COURT, PRATTVILLE,AL…
# $ `Zip Code`              <chr> "36067", NA, "36003", NA, "36567", NA, "3…
# $ `State Name`            <chr> "Alabama", "Alabama", "Alabama", "Alabama…
# $ `County Name`           <chr> "Autauga", "Autauga", "Autauga", "Baldwin…
# $ `City Name`             <chr> "Prattville", "Prattville", "Not in a Cit…
# $ `CBSA Name`             <chr> "Montgomery, AL", "Montgomery, AL", "Mont…
# $ `Tribe Name`            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ `Extraction Date`       <chr> "2021-05-18", "2021-05-18", "2021-05-18",…

# Create new names
oldNames <- names(aqs_sites)
firstChar <- stringr::str_sub(oldNames, 1, 1) %>% stringr::str_to_lower()
otherChars <- stringr::str_sub(oldNames, 2) %>% stringr::str_replace_all(" ", "")
newNames <- paste0("aqs_", firstChar, otherChars)

names(aqs_sites) <-
  newNames %>%
  # Fix lowercasing mistakes
  stringr::str_replace("aqs_gMTOffset", "aqs_GMTOffset") %>%
  stringr::str_replace("aqs_cBSAName", "aqs_CBSAName")

# Recipe for aqs_locationTbl
aqs_locationTbl <-

  # Start with aqs_sites
  aqs_sites %>%

  # Rename to match AirMonitor core metadata names
  dplyr::rename(
    longitude = aqs_longitude,
    latitude = aqs_latitude,
    elevation = aqs_elevation,
    stateCode = aqs_stateCode,
    countyName = aqs_countyName,
    locationName = aqs_localSiteName
  ) %>%

  # Restrict to North America valid locations
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
  dplyr::filter(longitude > -180 & longitude < -60 & latitude > 15 & latitude < 80) %>%

  # Create uniform casing/convert to POSIXct
  dplyr::mutate(
    locationName = stringr::str_to_sentence(locationName),
    aqs_address = stringr::str_to_sentence(aqs_address),
    # fix dates (local day begin ~=? east toast time)
    aqs_siteEstablishedDate = MazamaCoreUtils::parseDatetime(aqs_siteEstablishedDate, timezone = "America/New_York"),
    aqs_siteClosedDate = MazamaCoreUtils::parseDatetime(aqs_siteClosedDate, timezone = "America/New_York"),
    aqs_extractionDate = MazamaCoreUtils::parseDatetime(aqs_extractionDate, timezone = "America/New_York")
    ### elevation ft to meters???
  ) %>%

  # Create new core metadata variables
  dplyr::mutate(
    AQSID = paste0(stateCode, aqs_countyCode, aqs_siteNumber),
    locationID = MazamaCoreUtils::createLocationID(longitude, latitude),
  )





