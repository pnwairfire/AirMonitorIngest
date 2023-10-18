
# Package has functoins for interating with EPA AQS web pages
library(AirMonitorIngest)

# ----- Open up relevant web pages ---------------------------------------------
epa_aqs_openWebPages()

# From https://aqs.epa.gov/aqsweb/airdata/download_files.html
# we see that we want parameter codes "88101" and "88502"

# ----- Get monitor metadata for PM2.5 measurements only -----------------------

monitors <-
  epa_aqs_getMonitors() %>%
  dplyr::filter(`Parameter Code` %in% c("88101", "88502"))

# Review
dplyr::glimpse(monitors, width = 75)

# Rows: 5,412
# Columns: 32
# $ `State Code`                   <chr> "01", "01", "01", "01", "01", "01"…
# $ `County Code`                  <chr> "003", "005", "027", "033", "033",…
# $ `Site Number`                  <chr> "0010", "0002", "0001", "1002", "1…
# $ `Parameter Code`               <chr> "88101", "88502", "88101", "88101"…
# $ `Parameter Name`               <chr> "PM2.5 - Local Conditions", "Accep…
# $ POC                            <chr> "1", "5", "1", "1", "3", "1", "1",…
# $ Latitude                       <dbl> 30.49748, 31.66430, 33.28493, 34.7…
# $ Longitude                      <dbl> -87.88026, -85.60617, -85.80361, -…
# $ Datum                          <chr> "NAD83", "WGS84", "NAD83", "NAD83"…
# $ `First Year of Data`           <dbl> 2000, 2003, 1999, 1999, 2009, 1999…
# $ `Last Sample Date`             <chr> "2022-06-28", "2006-03-12", "2022-…
# $ `Monitor Type`                 <chr> "SLAMS", "SLAMS", "SLAMS", "SLAMS"…
# $ Networks                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ `Reporting Agency`             <chr> "Al Dept Of Env Mgt", "Research Tr…
# $ PQAO                           <chr> "Al Dept Of Env Mgt", "Research Tr…
# $ `Collecting Agency`            <chr> "Al Dept Of Env Mgt", "Al Dept Of …
# $ Exclusions                     <chr> NA, NA, NA, NA, "All (2009-04-03 -…
# $ `Monitoring Objective`         <chr> "POPULATION EXPOSURE", "POPULATION…
# $ `Last Method Code`             <chr> "145", "810", "145", "145", "170",…
# $ `Last Method`                  <chr> "R & P Model 2025 PM-2.5 Sequentia…
# $ `Measurement Scale`            <chr> "NEIGHBORHOOD", "REGIONAL SCALE", …
# $ `Measurement Scale Definition` <chr> "500 M TO 4KM", "50 TO HUNDREDS KM…
# $ `NAAQS Primary Monitor`        <chr> "Y", NA, "Y", NA, NA, "Y", NA, NA,…
# $ `QA Primary Monitor`           <chr> NA, NA, NA, NA, NA, NA, NA, NA, "Y…
# $ `Local Site Name`              <chr> "FAIRHOPE, Alabama", "CLIO", "ASHL…
# $ Address                        <chr> "FAIRHOPE HIGH SCHOOL, 1 PIRATE DR…
# $ `State Name`                   <chr> "Alabama", "Alabama", "Alabama", "…
# $ `County Name`                  <chr> "Baldwin", "Barbour", "Clay", "Col…
# $ `City Name`                    <chr> "Fairhope", "Not in a city", "Ashl…
# $ `CBSA Name`                    <chr> "Daphne-Fairhope-Foley, AL", "Eufa…
# $ `Tribe Name`                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ `Extraction Date`              <chr> "2022-11-14", "2022-11-14", "2022-…

# ----- Look at the `Last Method` to understand device type --------------------

table(monitors$`Last Method`) %>% sort(decreasing = TRUE)
#
# R & P Model 2025 PM-2.5 Sequential Air Sampler w/VSCC - Gravimetric
# 971
# R & P Model 2025 PM2.5 Sequential w/WINS - GRAVIMETRIC
# 638
# Met One BAM-1020 Mass Monitor w/VSCC - Beta Attenuation
# 470
# Met One SASS/SuperSASS Teflon - Gravimetric
# 375
# Teledyne T640 at 5.0 LPM - Broadband spectroscopy
# 266
# IMPROVE Module A with Cyclone Inlet-Teflon Filter, 2.2 sq. cm. - GRAVIMETRIC
# 255
# Met-One BAM-1020 W/PM2.5 SCC - Beta Attenuation
# 219
# PM2.5 SCC w/Correction Factor - TEOM Gravimetric 50 deg C
# 203
# R & P Model 2000 PM-2.5 Air Sampler w/VSCC - Gravimetric
# 168
# R & P Model 2000 PM2.5 Sampler w/WINS - GRAVIMETRIC
# 156
# Correlated Radiance Research M903 With Heated Inlet - Nephelometry
# 145
# Andersen RAAS2.5-300 PM2.5 SEQ w/WINS - GRAVIMETRIC
# 140
# ...

# ----- Create a key that can match airnow data to monitors --------------------

# The 9-digit AQSID is constructed in this manner
monitors$AQSID_9 <-
  sprintf(
    "%s%s%s",
    monitors$`State Code`,
    monitors$`County Code`,
    monitors$`Site Number`
  )

# NOTE that AirNow often uses 12-digit AQSID (aka fullAQSID) that has a country
# code attached. To have a unique key we can use to match AirNow monitors with
# the information in the above "monitors" table, we need to fix this.

airnow <- AirMonitor::airnow_loadLatest()

airnow$meta$AQSID %>% stringr::str_length() %>% table()
# .
# 9   12
# 1080  252

# Create a new variable so as not to overwrite existing variables
airnow$meta$AQSID_9 <- airnow$meta$AQSID
# Create a mask of all 12-character AQSIDs
mask_longAQSID <- stringr::str_length(airnow$meta$AQSID) == 12
# Shorton the 12-char AQSIDs to 9 characters
airnow$meta$AQSID_9[mask_longAQSID] <-
  airnow$meta$AQSID_9[mask_longAQSID] %>%
  stringr::str_sub(start = 4)

# ----- Example: find monitor type for an airnow monitor -----------------------

id <- airnow$meta$AQSID[234] # choosing a random one

monitors %>%
  dplyr::filter(AQSID_9 == !!id) %>%
  dplyr::glimpse()

# NOTE:  We managed to hit a site where 88101 uses a BAM-1022 and 88502 uses
# NOTE:  something else

# Try another

id <- airnow$meta$AQSID[816] # choosing a random one

monitors %>%
  dplyr::filter(AQSID_9 == !!id) %>%
  dplyr::glimpse()

# NOTE:  Here, both instruments are BAM-1020s

