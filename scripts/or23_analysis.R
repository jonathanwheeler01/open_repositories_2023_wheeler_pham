library(WDI)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyr)

## Read the aggregated data output by 'aggregate_raw_RAMP_data.py.'
ramp <- read_csv('./raw_data/ramp_sample_2019-2021.csv')

## Import the global region code lookup table.
globalns <- read_csv('./supplemental_data/north_south.csv')

# Convert country codes to lowercase.
# They are lowercase in the RAMP data, so this allows us to merge.
globalns$Countryabbre <- tolower(globalns$Countryabbre)

## Merge the datasets.
## Some rows will be dropped due to the use of 'unknown region' codes
## xkk and zzz in the RAMP data that cannot be joined to the regional
## country coded lookup table.
ramp <- merge(ramp, globalns,
                          by.x = "country", by.y = "Countryabbre") %>% 
  rename(region = Location, iso3c = country) %>% 
  mutate(iso3c = toupper(iso3c), year = year(date))

## For our initial overview, we use RAMP data by itself to demonstrate 
## trends by device type during COVID.

## Get the number of IR in the sample
## Data were cleaned and aggregated using Python
## The sample includes only IR that were in RAMP
## as of Jan 1, 2019
ir_sample <- unique(ramp$repository_id) # 45

## repository_id is otherwise unused
## remove this column
ramp <- ramp %>% 
  select(-repository_id)

## convert region to a factor with labels
## 1 == Global South, 2 == Global North
ramp$region <- as.factor(ramp$region)
levels(ramp$region)[1] <- 'Global South'
levels(ramp$region)[2] <- 'Global North'

# Plot total clicks per day
# with moving average at week (7), month (30), quarter (90)
total_clicks_per_day <- ramp %>% 
  select(date, clicks) %>% 
  group_by(date) %>% 
  summarise(total_clicks = sum(clicks)) %>% 
  mutate(avg_clicks_week = zoo::rollmean(total_clicks, k = 7, fill = NA),
         avg_clicks_month = zoo::rollmean(total_clicks, k = 30, fill = NA),
         avg_clicks_quarter = zoo::rollmean(total_clicks, k = 90, fill = NA)
  ) %>% 
  pivot_longer(names_to = "date_range",
               values_to = "sum_clicks",
               cols = c(total_clicks,
                        avg_clicks_week,
                        avg_clicks_month,
                        avg_clicks_quarter)) %>% 
  ggplot(aes(x = date, y = sum_clicks, color = date_range)) +
  geom_line(mapping = aes(linewidth = date_range,
                          alpha = date_range)) +
  scale_color_manual(breaks = c("avg_clicks_week",
                                "avg_clicks_month",
                                "avg_clicks_quarter"),
                     labels = c("7 day", "30 day", "90 day"),
                     values = c("firebrick1", "blue4", "yellow4", "black")) +
  scale_linewidth_manual(values = c(1.2, 1.2, 1.2, .5),
                         guide = FALSE) +
  scale_alpha_manual(values = c(.7, .7, .7, .4),
                     guide = FALSE) +
  labs(title = "Total clicks per day (2019-2021)",
       subtitle = "RAMP participating institutional repositories",
       x = element_blank(),
       y = element_blank(),
       color = "Rolling averages") +
  theme_linedraw()

## Going to use the 30 day average throughout
## 30 day is smooth but not too smooth
## Total clicks per day, 30 day rolling average
## faceted by global region
total_clicks_30day_ma_region <- ramp %>% 
  select(date, clicks, region) %>% 
  group_by(region, date) %>% 
  summarise(total_clicks = sum(clicks)) %>% 
  mutate(avg_clicks_month = zoo::rollmean(total_clicks, k = 30, fill = NA)) %>% 
  pivot_longer(names_to = "date_range",
               values_to = "sum_clicks",
               cols = c(total_clicks, avg_clicks_month)) %>% 
  ggplot(aes(x = date, y = sum_clicks, color = date_range)) +
  geom_line(mapping = aes(linewidth = date_range,
                          alpha = date_range)) +
  scale_color_manual(breaks = c("avg_clicks_month"),
                     labels = c("30 day"),
                     values = c("red", "black")) +
  scale_linewidth_manual(values = c(1, .5),
                         guide = FALSE) +
  scale_alpha_manual(values = c(.9, .4),
                     guide = FALSE) +
  facet_wrap(facets = vars(region)) +
  labs(title = "Total clicks per day (2019-2021)",
       subtitle = "By global region",
       x = element_blank(),
       y = element_blank(),
       color = "Rolling average") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Above, filtered to just 2020
total_clicks_30day_ma_region_2020 <- ramp %>% 
  select(date, clicks, region, year) %>% 
  group_by(year, region, date) %>% 
  filter(year == "2020") %>% 
  summarise(total_clicks = sum(clicks)) %>% 
  mutate(avg_clicks_month = zoo::rollmean(total_clicks, k = 30, fill = NA)) %>% 
  pivot_longer(names_to = "date_range",
               values_to = "sum_clicks",
               cols = c(total_clicks, avg_clicks_month)) %>% 
  ggplot(aes(x = date, y = sum_clicks, color = date_range)) +
  geom_line(mapping = aes(linewidth = date_range,
                          alpha = date_range)) +
  scale_color_manual(breaks = c("avg_clicks_month"),
                     labels = c("30 day"),
                     values = c("red", "black")) +
  scale_linewidth_manual(values = c(1, .5),
                         guide = FALSE) +
  scale_alpha_manual(values = c(.9, .4),
                     guide = FALSE) +
  facet_wrap(facets = vars(region)) +
  labs(title = "Total clicks per day (2020)",
       subtitle = "By global region",
       x = element_blank(),
       y = element_blank(),
       color = "Rolling average") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## The full 2019-2021 time series
## clicks by device
## faceted by region
clicks_device_region_all <- ramp %>% 
  select(date, clicks, region, device) %>% 
  group_by(region, device, date) %>% 
  summarise(total_clicks = sum(clicks)) %>% 
  mutate(avg_clicks_month = zoo::rollmean(total_clicks, k = 30, fill = NA)) %>% 
  pivot_longer(names_to = "date_range",
               values_to = "sum_clicks",
               cols = c(total_clicks, avg_clicks_month)) %>% 
  ggplot(aes(x = date, y = sum_clicks, color = date_range)) +
  geom_line(mapping = aes(linewidth = date_range,
                          alpha = date_range)) +
  scale_color_manual(breaks = c("avg_clicks_month"),
                     labels = c("30 day"),
                     values = c("red", "black")) +
  scale_linewidth_manual(values = c(.75, .5),
                         guide = FALSE) +
  scale_alpha_manual(values = c(.8, .6),
                     guide = FALSE) +
  facet_wrap(facets = vars(region, device)) +
  labs(title = "Total clicks per day (2019-2021)",
       subtitle = "By global region and device",
       x = element_blank(),
       y = element_blank(),
       color = "Rolling average") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## As above, filtered to 2020
## faceted by device and region
clicks_device_region_2020 <- ramp %>% 
  select(date, clicks, region, device, year) %>% 
  group_by(year, region, device, date) %>% 
  filter(year == "2020") %>% 
  summarise(total_clicks = sum(clicks)) %>% 
  mutate(avg_clicks_month = zoo::rollmean(total_clicks, k = 30, fill = NA)) %>% 
  pivot_longer(names_to = "date_range",
               values_to = "sum_clicks",
               cols = c(total_clicks, avg_clicks_month)) %>% 
  ggplot(aes(x = date, y = sum_clicks, color = date_range)) +
  geom_line(mapping = aes(linewidth = date_range,
                          alpha = date_range)) +
  scale_color_manual(breaks = c("avg_clicks_month"),
                     labels = c("30 day"),
                     values = c("red", "black")) +
  scale_linewidth_manual(values = c(.75, .5),
                         guide = FALSE) +
  scale_alpha_manual(values = c(.8, .6),
                     guide = FALSE) +
  facet_wrap(facets = vars(region, device)) +
  labs(title = "Total clicks per day (2020)",
       subtitle = "By global region and device",
       x = element_blank(),
       y = element_blank(),
       color = "Rolling average") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## A previous analysis on a smaller dataset
## indicated that users in the global south
## are more likely to use mobile devices
## than users from the global north.
## This is evident from above, but
## we now make this comparison with the larger
## dataset and augment with World Bank population data
# Get World Bank population data

# Web search gives the following as total population indicator
# https://data.worldbank.org/indicator/SP.POP.TOTL
world_pop <- WDI(indicator = c(population = 'SP.POP.TOTL'),
                 start = 2019,
                 end = 2021)

# need to add region data to population data
global_region <- read_csv('./supplemental_data/north_south.csv')

# merge region data to population data
# drop columns we won't use
pop_region <- merge(world_pop, global_region, 
                    by.x = "iso3c",
                    by.y = "Countryabbre") %>% 
  select(country, iso3c, year, population,
         Location) %>% 
  rename(region = Location)

# make Location a factor
pop_region$region <- as.factor(pop_region$region)
levels(pop_region$region)[1] <- 'Global South'
levels(pop_region$region)[2] <- 'Global North'

# join RAMP data with population data
ramp_pop <- inner_join(ramp, world_pop,
                       by = c("iso3c", "year"))

# We can look at clicks by device and by region
clicks_by_device_region <- ramp_pop %>% 
  select(region, population, device, clicks, year) %>% 
  group_by(region, device, population, year) %>% 
  summarise(clicks = sum(clicks)) %>% 
  group_by(region, device, year) %>% 
  summarize(pop = sum(population),
            clicks = sum(clicks)) %>% 
  mutate(clicks_weighted = (clicks/pop)*100) %>%
  arrange(desc(region))

# Note we are using population data to allow us
# to derive population weighted click values
# First, plot using unweighted values
options(scipen = 999)
unweighted_clicks_device <- clicks_by_device_region %>% 
  ggplot(aes(x = year, y = clicks, color = region)) +
  geom_line(linewidth = 1) +
  facet_wrap(facets = vars(device)) +
  labs(title = "Clicks by device (2019-2021)",
       x = element_blank(),
       y = element_blank(),
       color = "Region") +
  theme_linedraw() +
  theme(axis.text.x = element_blank())

# Now use clicks weighted by population
weighted_clicks_device <- clicks_by_device_region %>% 
  ggplot(aes(x = year, y = clicks_weighted, color = region)) +
  geom_line(linewidth = 1) +
  facet_wrap(facets = vars(device)) +
  labs(title = "Clicks by device (2019-2021)",
       subtitle = "Weighted by population (per 100 people)",
       x = element_blank(),
       y = element_blank(),
       color = "Region") +
  theme_linedraw() +
  theme(axis.text.x = element_blank())

## Problem with the above is the population difference
## between regions biases population weighted clicks
## at a global scale in favor of the global north.

## We can look more closely at this by limiting
## analysis to top 10 from each region
## Plot change in clicks relative to change in population
##---top 10 countries which generate most clicks
# not weighted by population
# seems we need a function to do this by region and year
top_10 <- function(y, r) {
  t10 <- ramp_pop %>%
    filter(year == y, region == r) %>% 
    select(clicks, Country, region, device, year) %>%
    group_by(Country, region, year, device) %>%
    summarise(clicks = sum(clicks)) %>%
    slice_max(clicks, n = 10) %>% 
    arrange(desc(clicks))
  return(head(t10, 10))
}

gn2019t10 <- top_10("2019", "Global North")
gn2020t10 <- top_10("2020", "Global North")
gn2021t10 <- top_10("2021", "Global North")
gs2019t10 <- top_10("2019", "Global South")
gs2020t10 <- top_10("2020", "Global South")
gs2021t10 <- top_10("2021", "Global South")

t10All <- gn2019t10 %>% 
  bind_rows(gn2020t10, gn2021t10, gs2019t10, gs2020t10, gs2021t10)

## plot top 10 countries in both regions
## not weighted by population
t10All_plot <- t10All %>%
  select(region, year, device, clicks) %>%
  group_by(region, year, device) %>% 
  summarise(total_clicks = sum(clicks)) %>% 
  ggplot(aes(x = year, y = total_clicks, color = region)) +
  geom_line(linewidth = 1, aes(group = region)) +
  facet_wrap(facets = vars(device)) +
  labs(title = "Total click activity by device (2019-2021)",
       subtitle = "Click activity limited to top 10 countries by region",
       x = element_blank(),
       y = element_blank(),
       color = "Region") +
  theme_linedraw() +
  theme(axis.text.x = element_blank())

# weighted by population
top_10_pop_wt <- function(y, r) {
  t10pwt <- ramp_pop%>%
    filter(region == r, year == y) %>% 
    mutate(click_weighted = (clicks/population)*100) %>% 
    select(clicks, Country, population, region, 
           click_weighted, year, device)%>%
    group_by(Country, region, year, population, device)%>%
    summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
    slice_max(clicks_weighted, n = 10) %>% 
    arrange(desc(clicks_weighted))
  return(head(t10pwt, 10))
}

gn2019t10pwt <- top_10_pop_wt("2019", "Global North")
gn2020t10pwt <- top_10_pop_wt("2020", "Global North")
gn2021t10pwt <- top_10_pop_wt("2021", "Global North")
gs2019t10pwt <- top_10_pop_wt("2019", "Global South")
gs2020t10pwt <- top_10_pop_wt("2020", "Global South")
gs2021t10pwt <- top_10_pop_wt("2021", "Global South")

t10Allpwt <- gn2019t10pwt %>% 
  bind_rows(gn2020t10pwt, gn2021t10pwt, gs2019t10pwt, 
            gs2020t10pwt, gs2021t10pwt)

## plot top 10 countries in both regions
## this time weighted by population
t10Allpwt_plot <- t10Allpwt %>%
  select(region, year, device, clicks_weighted) %>%
  group_by(region, year, device) %>% 
  summarise(total_clicks = sum(clicks_weighted)) %>% 
  ggplot(aes(x = year, y = total_clicks, color = region)) +
  geom_line(linewidth = 1, aes(group = region)) +
  facet_wrap(facets = vars(device)) +
  labs(title = "Population-weighted click activity by device (2019-2021)",
       subtitle = "Click activity limited to top 10 countries by region",
       x = element_blank(),
       y = element_blank(),
       color = "Region") +
  theme_linedraw() +
  theme(axis.text.x = element_blank())

