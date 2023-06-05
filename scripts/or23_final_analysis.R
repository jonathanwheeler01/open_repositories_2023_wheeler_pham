library(WDI)
library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
library(zoo)

## For our initial overview, we use RAMP data by itself to demonstrate 
## trends by device type during COVID.

# Read aggregated RAMP data for country and device access
# 2019-01-01 through 2021-12-31
ramp <- read_csv("./raw_data/ramp_country_coded_2019-2021.csv") %>% 
  rename(region = Location, iso3c = country) %>% 
  mutate(iso3c = toupper(iso3c), year = year(date))

## convert region to a factor with labels
## 1 == Global South, 2 == Global North
ramp$region <- as.factor(ramp$region)
levels(ramp$region)[1] <- 'Global South'
levels(ramp$region)[2] <- 'Global North'

## Get the number of IR in the sample
## Data were cleaned and aggregated using Python
## The sample includes only IR that were in RAMP
## as of Jan 1, 2019
ir_sample <- unique(ramp$repository_id) # 45

# Plot total clicks per day
# with moving average at week (7), month (30), quarter (90)
ramp %>% 
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
  theme_light()

## Going to use the 30 day average throughout
## 30 day is smooth but not too smooth
## Total clicks per day, 30 day rolling average
## faceted by global region
ramp %>% 
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
  theme_light()


## Above, filtered to just 2020
ramp %>% 
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
  theme_light()
