#library(WDI)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
#library(zoo)
#library(tidyr)

## Read the aggregated data output by 'aggregate_raw_RAMP_data.py.'
#ramp <- read_csv('./raw_data/ramp_sample_2019-2021.csv')

## Import the global region code lookup table.
#globalns <- read_csv('./supplemental_data/north_south.csv')

# Convert country codes to lowercase.
# They are lowercase in the RAMP data, so this allows us to merge.
#globalns$Countryabbre <- tolower(globalns$Countryabbre)

## Merge the datasets.
## Some rows will be dropped due to the use of 'unknown region' codes
## xkk and zzz in the RAMP data that cannot be joined to the regional
## country coded lookup table.
#ramp <- merge(ramp, globalns,
#                          by.x = "country", by.y = "Countryabbre") %>% 
#  rename(region = Location, iso3c = country) %>% 
#  mutate(iso3c = toupper(iso3c), year = year(date))

## Alternatively country-coded data can be saved to a file and
## imported for additional analyses. This prevents having to
## execute the above code every time the script is run.
ramp <- read_csv('./raw_data/ramp_country_coded_2019-2021.csv') %>% 
  rename(region = Location, iso3c = country) %>% 
  mutate(iso3c = toupper(iso3c), year = year(date))

## repository_id is otherwise unused
## remove this column

ramp <- ramp %>% 
  select(-repository_id)

## convert region to a factor with labels
## 1 == Global South, 2 == Global North
ramp$region <- as.factor(ramp$region)
levels(ramp$region)[1] <- 'Global South'
levels(ramp$region)[2] <- 'Global North'

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
#world_pop <- WDI(indicator = c(population = 'SP.POP.TOTL'),
#                 start = 2019,
#                 end = 2021)

world_pop <- read_csv("./supplemental_data/wb_population.csv")
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

## position vs. clicks table
#library(psych)
## Add clicks_weighted to ramp_pop
ramp_pop$clicks_weighted <-  (ramp_pop$clicks/ramp_pop$population)*100

rm(global_region)
rm(pop_region)
rm(ramp)
rm(world_pop)
gc()
# Try plotting clicks and weighted clicks by position, device, region
# Add regression lines to the below
clicks_device_position <- ramp_pop %>% 
  filter(device != "TABLET", region == "Global South",
         clicks > 0) %>% 
  select(position, clicks, device, region)

cdp_plot <- ggplot(clicks_device_position, 
                   aes(x = position, y = clicks))

cdp_plot_point <-  cdp_plot + geom_point(aes(color = device),
                                         alpha = 0.5)

rm(cdp_plot)
cdp_plot_smooth <- cdp_plot_point + geom_smooth(linewidth = 1,
                                                method = "lm",
                                                se = FALSE,
                                                color = "red")

rm(cdp_plot_point)  
cdp_plot_facet <-  cdp_plot_smooth + facet_wrap(facets = vars(region, device))

rm(cdp_plot_smooth)
cdp_plot_labels <- cdp_plot_facet + labs(title = "Clicks relative to position",
                                         subtitle = "Global South by device",
                                         x = "Position",
                                         y = "Total clicks")

rm(cdp_plot_facet)
cdp_plot_theme <- cdp_plot_labels + theme_linedraw()

rm(cdp_plot_labels)
ggsave("clicks_device_pos_gs.png", width = 5, height = 5)

rm(cdp_plot_theme)
rm(clicks_device_position)
gc()

weighted_clicks_device_position <- ramp_pop %>% 
  filter(device != "TABLET", region == "Global South",
         clicks_weighted > 0) %>% 
  select(position, clicks_weighted, device, region)

wcdp_plot <- ggplot(weighted_clicks_device_position,
                    aes(x = position, y = clicks_weighted))

wcdp_plot + geom_point(aes(color = device),
                       alpha = 0.5) +
  geom_smooth(linewidth = 1, method = "lm", se = FALSE, color = "red") +
  facet_wrap(facets = vars(region, device)) +
  labs(title = "Population weighted clicks relative to position",
       subtitle = "Global South by device",
       x = "Position",
       y = "Total clicks") + 
  theme_linedraw()
ggsave("wghtd_clicks_device_pos_gs.png", width = 5, height = 5)

rm(wcdp_plot)
rm(weighted_clicks_device_position)
gc()


