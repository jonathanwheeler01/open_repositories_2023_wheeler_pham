library(WDI)
library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)

# Get World Bank population data

# Web search gives the following as total population indicator
# https://data.worldbank.org/indicator/SP.POP.TOTL
world_pop <- WDI(indicator = c(population = 'SP.POP.TOTL'),
                 start = 2019,
                 end = 2021)

global_region <- read_csv('../supplemental_data/north_south.csv')

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

# convert year
pop_region$year <- year(as_date(as.character(pop_region$year),
                                format="%Y"))
pop_region$y_fac <- as.factor(pop_region$year)
pop_region$y_char <- as.character(pop_region$year)


# total pop per year per region
# this is the "ground truth" pop totals for 
# analysis with RAMP data
pop_region_year <- pop_region %>% 
  select(region, y_char, population) %>% 
  group_by(region, y_char) %>% 
  summarise(regional_population = sum(population))

# not a very interesting plot
#ggplot(pop_region_year, aes(y_char, regional_population,
#                            group = region,
#                            color = region)) +
#  geom_line()

# read the RAMP data for country and device access
# 2019-01-01 through 2021-12-31
ramp <- read_csv("../raw_data/ramp_country_coded_2019-2021.csv") %>% 
  rename(region = Location, iso3c = country) %>% 
  mutate(iso3c = toupper(iso3c))

# make Location a factor
ramp$region <- as.factor(ramp$region)
levels(ramp$region)[1] <- 'Global South'
levels(ramp$region)[2] <- 'Global North'


# subset RAMP by year
library(lubridate)
ramp2019 <-  ramp %>% 
  filter(year(ramp$date) == 2019)

ramp2020 <-  ramp %>% 
  filter(year(ramp$date) == 2020)

ramp2021 <-  ramp %>% 
  filter(year(ramp$date) == 2021)

# subset world_pop by year
pop2019 <-  world_pop %>% 
  filter(world_pop$year == 2019)

pop2020 <-  world_pop %>% 
  filter(world_pop$year == 2020)

pop2021 <-  world_pop %>% 
  filter(world_pop$year == 2021)

# join RAMP data to population data
ramp2019_pop <- inner_join(ramp2019,
                           pop2019,
                           by = "iso3c")

ramp2020_pop <- inner_join(ramp2020,
                           pop2020,
                           by = "iso3c")

ramp2021_pop <- inner_join(ramp2021,
                           pop2021,
                           by = "iso3c")

# aggregate by country/device combination
# and calculate clicks weighted by population
# "clicks_weighted is expressed as a percentage
# of the population who accessed IR content
# using the corresponding device
clicks_by_device_country_2019 <- ramp2019_pop %>% 
  select(clicks, Country, region, population, device) %>% 
  group_by(Country, region, device, population) %>% 
  summarise(clicks = sum(clicks)) %>% 
  mutate(clicks_weighted = (clicks/population)*100) %>% 
  arrange(desc(clicks_weighted))

clicks_by_device_country_2020 <- ramp2020_pop %>% 
  select(clicks, Country, region, population, device) %>% 
  group_by(Country, region, device, population) %>% 
  summarise(clicks = sum(clicks)) %>% 
  mutate(clicks_weighted = (clicks/population)*100) %>% 
  arrange(desc(clicks_weighted))

clicks_by_device_country_2021 <- ramp2021_pop %>% 
  select(clicks, Country, region, population, device) %>% 
  group_by(Country, region, device, population) %>% 
  summarise(clicks = sum(clicks)) %>% 
  mutate(clicks_weighted = (clicks/population)*100) %>% 
  arrange(desc(clicks_weighted))

# same as above, by region not country
clicks_by_device_region_2019 <- clicks_by_device_country_2019 %>% 
  select(region, population, device, clicks) %>% 
  group_by(region, device) %>% 
  summarize(pop = sum(population),
            clicks = sum(clicks)) %>% 
  mutate(clicks_weighted = (clicks/pop)*100) %>% 
  arrange(desc(region))

clicks_by_device_region_2020 <- clicks_by_device_country_2020 %>% 
  select(region, population, device, clicks) %>% 
  group_by(region, device) %>% 
  summarize(pop = sum(population),
            clicks = sum(clicks)) %>% 
  mutate(clicks_weighted = (clicks/pop)*100) %>% 
  arrange(desc(region))

clicks_by_device_region_2021 <- clicks_by_device_country_2021 %>% 
  select(region, population, device, clicks) %>% 
  group_by(region, device) %>% 
  summarize(pop = sum(population),
            clicks = sum(clicks)) %>% 
  mutate(clicks_weighted = (clicks/pop)*100) %>% 
  arrange(desc(region))


# ----------------PART I: Global patterns in accessing academic knowledge
##---------------- clicks from users in the global north and south
clicks_region_2019 <- ramp2019_pop %>% 
  select(clicks, region) %>% 
  group_by(region) %>% 
  summarise(clicks = sum(clicks)) %>% 
  mutate(percent = clicks/sum(clicks)*100)

clicks_region_2020 <- ramp2020_pop %>% 
  select(clicks, region) %>% 
  group_by(region) %>% 
  summarise(clicks = sum(clicks)) %>% 
  mutate(percent = clicks/sum(clicks)*100)

clicks_region_2021 <- ramp2021_pop %>% 
  select(clicks, region) %>% 
  group_by(region) %>% 
  summarise(clicks = sum(clicks)) %>% 
  mutate(percent = clicks/sum(clicks)*100)


##--------------average clicks per country in the global north and the 
## global south. This is the avg clicks per country in each region

# Leaving this out of OR23 analysis

country_click_2019 <- ramp2019_pop %>%
  select(clicks, Country, region)%>%
  group_by(Country, region)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks)) %>% 
  group_by(region) %>% 
  mutate(mean_click = mean(clicks))

country_click_2020 <- ramp2020_pop %>%
  select(clicks, Country, region)%>%
  group_by(Country, region)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks)) %>% 
  group_by(region) %>% 
  mutate(mean_click = mean(clicks))

country_click_2021 <- ramp2021_pop %>%
  select(clicks, Country, region)%>%
  group_by(Country, region)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks)) %>% 
  group_by(region) %>% 
  mutate(mean_click = mean(clicks))

# Maybe a different way to look at the above
ramp2019_pop %>%
  select(clicks, Country, region)%>%
  group_by(Country, region)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks)) %>% 
  group_by(region) %>%
  filter(region == "Global North") %>% 
  summary()

ramp2019_pop %>%
  select(clicks, Country, region)%>%
  group_by(Country, region)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks)) %>% 
  group_by(region) %>%
  filter(region == "Global South") %>% 
  summary()


##--------------average clicks per population in the global north 
## and the global south
ramp2019_pop %>%
  select(clicks, Country, region, population)%>%
  group_by(Country, region, population)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  mutate(mean_click = clicks/population)%>%
  group_by(region)%>%
  filter(region == "Global South") %>% 
  summary()
  
ramp2020_pop %>%
  select(clicks, Country, region, population)%>%
  group_by(Country, region, population)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  mutate(mean_click = clicks/population)%>%
  group_by(region)%>%
  filter(region == "Global South") %>% 
  summary()

ramp2021_pop %>%
  select(clicks, Country, region, population)%>%
  group_by(Country, region, population)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  mutate(mean_click = clicks/population)%>%
  group_by(region)%>%
  filter(region == "Global South") %>% 
  summary()

## Plot change in clicks relative to change in population
##---top 10 countries which generate most clicks
# not weighted by population
head(ramp2021_pop%>%
       select(clicks, Country, region)%>%
       group_by(Country, region)%>%
       summarise(clicks = sum(clicks))%>%
       arrange(desc(clicks)),10)

# weighted by population
top_10_n <- head(ramp2019_pop%>%
                   mutate(click_weighted = (clicks/population)*100,
                          year = 2019) %>% 
                   select(clicks, Country, population, region, 
                          click_weighted, year)%>%
                   group_by(Country, population, region, year)%>%
                   filter(region == "Global North") %>% 
                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                   arrange(desc(clicks_weighted)),10)

top_10_n <- rbind(top_10_n, head(ramp2020_pop%>%
                                   mutate(click_weighted = (clicks/population)*100,
                                          year = 2020) %>% 
                                   select(clicks, Country, population, region,
                                          click_weighted, year)%>%
                                   group_by(Country, population, region, year)%>%
                                   filter(region == "Global North") %>% 
                                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                                   arrange(desc(clicks_weighted)),10))

top_10_n <- rbind(top_10_n, head(ramp2021_pop%>%
                                   mutate(click_weighted = (clicks/population)*100,
                                          year - 2021) %>% 
                                   select(clicks, Country, population, region,
                                          click_weighted, year) %>%
                                   group_by(Country, population, region, year)%>%
                                   filter(region == "Global North") %>% 
                                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                                   arrange(desc(clicks_weighted)),10))



top_10_s <- head(ramp2019_pop%>%
                 mutate(click_weighted = (clicks/population)*100,
                        year = 2019) %>% 
                 select(clicks, Country, population, region, 
                        click_weighted, year)%>%
                 group_by(Country, population, region, year)%>%
                 filter(region == "Global South") %>% 
                 summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                 arrange(desc(clicks_weighted)),10)

top_10_s <- rbind(top_10_s, head(ramp2020_pop%>%
                            mutate(click_weighted = (clicks/population)*100,
                                   year = 2020) %>% 
                            select(clicks, Country, population, region,
                                   click_weighted, year)%>%
                            group_by(Country, population, region, year)%>%
                            filter(region == "Global South") %>% 
                            summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                            arrange(desc(clicks_weighted)),10))

top_10_s <- rbind(top_10_s, head(ramp2021_pop%>%
                            mutate(click_weighted = (clicks/population)*100,
                                   year - 2021) %>% 
                            select(clicks, Country, population, region,
                                   click_weighted, year) %>%
                            group_by(Country, population, region, year)%>%
                            filter(region == "Global South") %>% 
                            summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                            arrange(desc(clicks_weighted)),10))

## plot
ggplot(top_10_n, mapping = aes(x = year, 
                               y = clicks_weighted,
                               color = Country)) +
  geom_line(size = 1.5)

ggplot(top_10_s, mapping = aes(x = year, 
                               y = clicks_weighted,
                               color = Country)) +
  geom_line(size = 1.5)


# this gets them all into 1 plot but is hard to read
top_10_ns <- rbind(top_10_n, top_10_s)
ggplot(top_10_ns, mapping = aes(x = year, 
                               y = clicks_weighted,
                               color = Country)) +
  geom_line(size = 1.5)


# same for top 5
top_5_n <- head(ramp2019_pop%>%
                   mutate(click_weighted = (clicks/population)*100,
                          year = 2019) %>% 
                   select(clicks, Country, population, region, 
                          click_weighted, year)%>%
                   group_by(Country, population, region, year)%>%
                   filter(region == "Global North") %>% 
                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                   arrange(desc(clicks_weighted)),5)

top_5_n <- rbind(top_5_n, head(ramp2020_pop%>%
                                   mutate(click_weighted = (clicks/population)*100,
                                          year = 2020) %>% 
                                   select(clicks, Country, population, region,
                                          click_weighted, year)%>%
                                   group_by(Country, population, region, year)%>%
                                   filter(region == "Global North") %>% 
                                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                                   arrange(desc(clicks_weighted)),5))

top_5_n <- rbind(top_5_n, head(ramp2021_pop%>%
                                   mutate(click_weighted = (clicks/population)*100,
                                          year - 2021) %>% 
                                   select(clicks, Country, population, region,
                                          click_weighted, year) %>%
                                   group_by(Country, population, region, year)%>%
                                   filter(region == "Global North") %>% 
                                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                                   arrange(desc(clicks_weighted)),5))



top_5_s <- head(ramp2019_pop%>%
                   mutate(click_weighted = (clicks/population)*100,
                          year = 2019) %>% 
                   select(clicks, Country, population, region, 
                          click_weighted, year)%>%
                   group_by(Country, population, region, year)%>%
                   filter(region == "Global South") %>% 
                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                   arrange(desc(clicks_weighted)),5)

top_5_s <- rbind(top_5_s, head(ramp2020_pop%>%
                                   mutate(click_weighted = (clicks/population)*100,
                                          year = 2020) %>% 
                                   select(clicks, Country, population, region,
                                          click_weighted, year)%>%
                                   group_by(Country, population, region, year)%>%
                                   filter(region == "Global South") %>% 
                                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                                   arrange(desc(clicks_weighted)),5))

top_5_s <- rbind(top_5_s, head(ramp2021_pop%>%
                                   mutate(click_weighted = (clicks/population)*100,
                                          year - 2021) %>% 
                                   select(clicks, Country, population, region,
                                          click_weighted, year) %>%
                                   group_by(Country, population, region, year)%>%
                                   filter(region == "Global South") %>% 
                                   summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
                                   arrange(desc(clicks_weighted)),5))

## plot
ggplot(top_5_n, mapping = aes(x = year, 
                               y = clicks_weighted,
                               color = Country)) +
  geom_line(size = 1.5)

ggplot(top_5_s, mapping = aes(x = year, 
                               y = clicks_weighted,
                               color = Country)) +
  geom_line(size = 1.5)


# this gets them all into 1 plot but is hard to read
top_5_ns <- rbind(top_5_n, top_5_s)
ggplot(top_5_ns, mapping = aes(x = year, 
                                y = clicks_weighted,
                                color = Country)) +
  geom_line(size = 1.5, aes(linetype = region))



##---bottom 10 countries which generate most clicks
# not weighted by population
tail(ramp2019_pop%>%
       select(clicks, Country, region)%>%
       group_by(Country, region)%>%
       summarise(clicks = sum(clicks))%>%
       arrange(desc(clicks)),10)

# weighted by population
tail(ramp2019_pop%>%
       mutate(click_weighted = (clicks/population)*100) %>% 
       select(clicks, Country, population, region, click_weighted)%>%
       group_by(Country, population, region)%>%
       filter(region == "Global South") %>% 
       summarise(clicks_weighted = sum(click_weighted), clicks = sum(clicks))%>%
       arrange(desc(clicks_weighted)),10)


# ----------------PART II: Technologies & academics
##----------------country, device, click
country_click_device <- ramp2019_pop%>%
  select(clicks, Country, region, device)%>%
  group_by(Country, region, device)%>%
  summarise(click_s = sum(clicks))%>%
  arrange(desc(click_s))


##----------------region, device, click
region_click_device <- ramp2021_pop%>%
  select(clicks, region, device)%>%
  group_by(region, device)%>%
  summarise(click_s = sum(clicks))%>%
  arrange(desc(click_s))

#------------------PART III: Click position bias and beyond
click_position <- ramp2019_pop%>%
  filter(clicks>0)%>%
  select(position, device, clicks, region)

click_position_unclicked <- ramp2021_pop%>%
  filter(clicks==0)%>%
  select(position, device, region)


## position vs. clicks table
library(psych)
describeBy(ramp2019_pop$clicks, 
           list(ramp2019_pop$region,
                ramp2019_pop$device), mat=TRUE)
describeBy(ramp2019_pop$position, 
           list(ramp2019_pop$region, 
                ramp2019_pop$device), mat=TRUE)


## the above includes all rows with 0 clicks
## useful info but lets set a boolean for clicks > 0
ramp_clicked <- ramp2019_pop %>% filter(clicks > 0)
describeBy(ramp_clicked$clicks, 
           list(ramp_clicked$region, ramp_clicked$device), mat=TRUE)
describeBy(ramp_clicked$position, 
           list(ramp_clicked$region, ramp_clicked$device), mat=TRUE)
describeBy(ramp_clicked$position, 
           list(ramp_clicked$device), mat=TRUE)

ramp_unclicked <- ramp2019_pop %>% filter(clicks == 0)
describeBy(ramp_unclicked$position, 
           list(ramp_unclicked$region, ramp_unclicked$device), mat=TRUE)
describeBy(ramp_clicked$position, 
           list(ramp_clicked$region, ramp_clicked$device), mat=TRUE)
describeBy(ramp_unclicked$position, 
           list(ramp_unclicked$device), mat=TRUE)
describeBy(ramp_clicked$position, 
           list(ramp_clicked$device), mat=TRUE)

##-- position, click vs. location, clicked
ramp_clicked_south <- ramp2019_pop%>%
  filter(region=='Global South', clicks >0)

describeBy(ramp_clicked_south$position, 
           list(ramp_clicked_south$device), mat=TRUE)

ramp_clicked_north <- ramp2019_pop%>%
  filter(region=='Global North', clicks >0)

describeBy(ramp_clicked_north$position, 
           list(ramp_clicked_north$device), mat=TRUE)


##-- position, click vs. location, unclicked
ramp_unclicked_south <- ramp2019_pop%>%
  filter(region=='Global South', clicks==0)

describeBy(ramp_unclicked_south$position, 
           list(ramp_unclicked_south$device), mat=TRUE)

ramp_unclicked_north <- ramp2019_pop%>%
  filter(region=='Global North', clicks==0)

describeBy(ramp_unclicked_north$position, 
           list(ramp_unclicked_north$device), mat=TRUE)


# exploring correlation between pop number and click number: 
# weak but significant correlation with p smaller than .001
library("Hmisc")
cor(ramp2019_pop$position, 
    ramp2019_pop$clicks, method = c("spearman"))

m1 <- lm(clicks~region, data = country_click_2019)
summary(m1)

# original uses pop weighted clicks - would have to add
# that to use this
m2 <- lm(mean_click~region, data = country_click_2019)
summary(m2)
confint(m2)
anova(m2, test = "Chisq")

#finding correlation
head(ramp2019_pop)
ramp2019_pop$region_code <- as.numeric(ramp2019_pop$region)
corr <- round(cor(ramp2019_pop[,c(2, 5, 7, 15, 16)], use="pairwise.complete.obs"), 1)
library(ggcorrplot)
ggcorrplot(corr, 
           method = "circle",
           hc.order = TRUE,
           type = "upper",
           outline.color = "white")























