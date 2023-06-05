library(tidyverse)
library(rio)
library(psych)

# Read the data
ramp <- read_csv('./raw_data/ramp_sample_2019-2021.csv')

# Descriptive statistics
# Redoing tables from the article
## Caption "Breakdown of Clicks by Device"
ramp %>% 
  select(clicks, device) %>% 
  group_by(device) %>% 
  summarise(count = sum(!is.na(device)), clicks = sum(clicks)) %>% 
  arrange(desc(clicks)) %>% 
  mutate(percent = clicks/sum(clicks)*100)

## Caption "Positions vs. Clicks"
## Descriptive statistics for clicks by device.
describeBy(ramp$clicks, ramp$device)

## Descriptive statistics for position by device.
describeBy(ramp$position, ramp$device)

## Access by country, and devuce use across countries
## from the global north and global south
## Return the number of unique ISO three letter country codes in the RAMP data.
length(unique(ramp$country))

# 245 country codes in RAMP

## Import the global region code lookup table.
globalns <- read_csv('../supplemental_data/north_south.csv')
head(globalns)

# 249 country codes in global ns lookup table

# Convert country codes to lowercase.
# They are lowercase in the RAMP data, so this allows us to merge.

globalns$Countryabbre <- tolower(globalns$Countryabbre)

# Merge
country_device_n <- merge(ramp, globalns,
                          by.x = "country", by.y = "Countryabbre")
length(unique(country_device_n$country))

# 243 country codes in merged data

# QA on country codes
# Find codes in RAMP but not in globalns
ramp_codes <- unique(ramp$country)
global_codes <- unique(globalns$Countryabbre)
ramp_codes[!(ramp_codes %in% global_codes)]

# Same codes as previous analysis - xkk and zzz
# these are 'unknown region' codes

# Codes in globalbs but not in ramp
# This probably means there were no searches from these 
# countries that surfaced IR content in Google SERP
# "bvt" "cck" "atf" "hmd" "vat" "sgs"
# Generally small islands, and also the Vatican
not_in_ramp <- global_codes[!(global_codes %in% ramp_codes)]

# This is reasonable

# Analyze by country and device
country_device_n$Location <- as.factor(country_device_n$Location)

## Location codes: 1 == "Global North," 0 == "Global South"
country_device_n %>% 
  select(clicks, Location) %>% 
  group_by(Location) %>% 
  summarise(clicks = sum(clicks)) %>% 
  arrange(desc(clicks)) %>% 
  mutate(percent = clicks/sum(clicks)*100)

## Caption "Top five countries that generated IR traffic to RAMP-registered repositories"
country_device_n %>% select(clicks, country) %>% 
  group_by(country) %>% 
  summarise(clicks = sum(clicks)) %>% 
  arrange(desc(clicks)) %>% 
  head(5)

## Caption "Device use between users in the Global North and the Global South"
## Location codes: 1 == "Global North," 0 == "Global South"
country_device_n%>%
  select(clicks, device, Location)%>%
  group_by(Location, device)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  mutate(percent = clicks/sum(clicks)*100)

write_csv(country_device_n, '../raw_data/ramp_country_coded_2019-2021.csv')
