# Minh Pham & Jon Wheeler

#------------------
# import data
library(rio)
dat6 <- import('../supplemental_data/north_south.csv')
dat7 <- import('../supplemental_data/Pop by country.xlsx')

# head(dat6)
# head(dat7)

# subsetting columns to free memory
head(dat7)
dat7 <- dat7[, c(2, 10)]
# head(dat7)

#changing name to merge later on
names(dat7)[1] <- "Country"
names(dat7)[2] <- "pop2019"



# check matching countries
dat8 <- unique(dat6$Country)
dat9 <- unique(dat7$Country)
dat8[!(dat8 %in% dat9)]
dat9[!(dat9 %in% dat8)]

length(dat8)
length(dat9)
232 - 248 # 16 countries are not listed in Pop data

# merging two files, using pop file as the key file
North_south_pop <- dplyr::left_join(dat7, dat6, by ="Country")

#head(North_south_pop)
#nrow(North_south_pop)
#list(North_south_pop$Country)
#length(unique(North_south_pop$Country))

# finding duplicate # not necessary any more
# duplicated(North_south_pop$Country)
# North_south_pop$Country[duplicated(North_south_pop$Country)]

#remove unnecessary variables
#North_south_pop <- North_south_pop[, c(1,3:5)]

#changing string format to lower case to merge data later
North_south_pop$Countryabbre <- tolower(North_south_pop$Countryabbre)
head(North_south_pop)


## load data files

## concatenate tables and free some memory
## to work with 2019 population data, filter to just 2019
country_device <- read_csv('./raw_data/ramp_sample_2019-2021.csv')
head(country_device)
country_device <- country_device %>%
  filter(date >= "2019-01-01" & date <= "2019-12-31")
head(country_device)
tail(country_device)

## make sure we're using the subset (length should be 35)
length(unique(country_device$index))
head(country_device)

#remove unnecessary variables
country_device <- country_device[, c(1,3, 4, 7)]
#head(country_device)

# check how many countries there are in country_device
length(unique(country_device$country))

# check matching countries
dat10 <- unique(country_device$country)
dat11 <- unique(North_south_pop$Countryabbre)
dat10[!(dat10 %in% dat11)]
dat11[!(dat11 %in% dat10)]

which(North_south_pop[,3] == "vat")
North_south_pop[232,]

# total number of columns which might need to be removed
length(which(country_device[,2]== c("ggy", "xkk", "zzz", "shn", "ata",
                                    "ala", "jey", "sjm", "bes", "nfk",
                                    "iot", "cxr", "umi")))

length(dat10) - length(dat11)
 # 12 countries are not listed in Pop data
 # 1 country Vatican City is not listed in country_device

# fixing variable name to merge later
names(country_device)[2] <- "Countryabbre"

ramp <- dplyr::inner_join(country_device, North_south_pop, by = "Countryabbre")
head(ramp)
#str(ramp)

library(dplyr)
library(tidyr)

## set global region (0=south, 1=north) as factor
ramp$Location <- as.factor(ramp$Location)

# change levels of measurement for other variables as well
ramp$Countryabbre <- as.factor(ramp$Countryabbre)
ramp$device <- as.factor(ramp$device)
ramp$Country <- as.factor(ramp$Country)

#str(ramp)

#write.csv(ramp, file = "ramp.csv")

# normalizing click data
range(ramp$pop2019)
ramp$click_weighted <- (ramp$clicks/ramp$pop2019)*100 # total number of clicks per 100 people 

#preparing for finding cor later
ramp$device_n <- ifelse(ramp$device=="DESKTOP", 0,
                        ifelse(ramp$device=="MOBILE", 1, 0))
ramp$Location_n <- as.numeric(ramp$Location)

#str(ramp)
head(ramp, 20)
library(dplyr)
#ramp%>%
  #filter(Country=="Afghanistan")
ramp%>%
  select(clicks, Country, Location, pop2019, device)%>%
  group_by(Country, Location, device, pop2019)%>%
  summarise(clicks = sum(clicks))%>%
  mutate(click_weighted = (clicks/pop2019)*100)%>%
  arrange(desc(click_weighted))


ramp%>%
  select(click_weighted, Country, Location, device)%>%
  group_by(Country, Location, device)%>%
  summarise(click_weighted_s = sum(click_weighted))%>%
  arrange(desc(click_weighted_s))




# ----------------PART I: Global patterns in accessing academic knowledge
##---------------- clicks from users in the global north and south
clics_north_south_1 <- ramp%>%
  select(clicks,Location)%>%
  group_by(Location)%>%
  summarise(clicks = sum(clicks))%>%
  mutate(percent = clicks/sum(clicks)*100)
clics_north_south_1$Location <- ifelse(clics_north_south_1$Location=="1", "Global North", "Global South")
# write.csv(clics_north_south_1, file = "clics_north_south_1.csv")

##--------------average clicks per country in the global north and the global south
country_click <- ramp%>%
  select(clicks, Country, Location)%>%
  group_by(Country, Location)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  group_by(Location)%>%
  mutate(ave_click = mean(clicks))
#country_click$Location <- ifelse(country_click$Location=="1", "Global North", "Global South")
#write.csv(country_click, file = "country_click_un.csv")

##--------------average clicks per population in the global north and the global south
country_click_by_pop <- ramp%>%
  select(clicks, Country, Location, pop2019)%>%
  group_by(Country, Location, pop2019)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  mutate(ave_click = clicks/pop2019)%>%
  group_by(Location)%>%
  mutate(ave_click = mean(ave_click))
#country_click_by_pop$Location <- ifelse(country_click_by_pop$Location=="1", "Global North", "Global South")
#write.csv(country_click_by_pop, file = "country_click_by_pop.csv")

##---top 10 countries which generate most clicks: non-standardized
top_10_1 <- head(ramp%>%
                 select(clicks, Country, Location)%>%
                 group_by(Country, Location)%>%
                 summarise(clicks = sum(clicks))%>%
                 arrange(desc(clicks)),10)
#top_10_1$Location <- ifelse(top_10_1$Location=="1", "Global North", "Global South")
#write.csv(top_10_1, file = "top_10_1_non.csv")

##---top 10 countries which generate most clicks: standardized
top_10_2_s <- head(ramp%>%
                 select(clicks, Country, pop2019, Location, click_weighted)%>%
                 group_by(Country, pop2019, Location)%>%
                 summarise(clicks_s = sum(click_weighted), clicks = sum(clicks))%>%
                 arrange(desc(clicks_s)),10)
#top_10$Location <- ifelse(top_10$Location=="1", "Global North", "Global South")
#write.csv(top_10, file = "top_10.csv")


##---bottom 10 countries which generate the fewest clicks: non-standardized
bottom_10_1 <- tail(ramp%>%
       select(clicks, Country, Location)%>%
       group_by(Country, Location)%>%
       summarise(clicks = sum(clicks))%>%
       arrange(desc(clicks)), 10)
bottom_10_1$Location <- ifelse(bottom_10_1$Location=="1", "Global North", "Global South")
write.csv(bottom_10_1, file = "bottom_10_1_non.csv")


##---bottom 10 countries which generate the fewest clicks: standardized
bottom_10_2 <- tail(ramp%>%
       select(click_weighted, Country, Location)%>%
       group_by(Country, Location)%>%
       summarise(clicks = sum(click_weighted))%>%
       arrange(desc(clicks)), 10)
bottom_10_2$Location <- ifelse(bottom_10_2$Location=="1", "Global North", "Global South")
write.csv(bottom_10_2, file = "bottom_10_2_st.csv")

# ----------------PART II: Technologies & academics
##----------------country, device, click
country_click_device <- ramp%>%
  select(clicks, Country, Location, device)%>%
  group_by(Country, Location, device)%>%
  summarise(click_s = sum(clicks))%>%
  arrange(desc(click_s))
#country_click_device$Location <- ifelse(country_click_device$Location==1, "Global North", "Global South")
#write.csv(country_click_device, file = "country_click_device.csv")

##----------------region, device, click
region_click_device <- ramp%>%
  select(clicks, Location, device)%>%
  group_by(Location, device)%>%
  summarise(click_s = sum(clicks))%>%
  arrange(desc(click_s))

#region_click_device$Location <- ifelse(region_click_device$Location==1, "Global North", "Global South")
#write.csv(region_click_device, file = "region_click_device.csv")

#------------------PART III: Click position bias and beyond
click_position <- ramp%>%
  filter(clicks>0)%>%
  select(position, device, clicks, Location)

click_position$Location <- ifelse(click_position$Location==1, "Global North", "Global South")
write.csv(click_position, file = "click_position.csv")

click_position_unclicked <- ramp%>%
  filter(clicks==0)%>%
  select(position, device, Location)

click_position_unclicked$Location <- ifelse(click_position_unclicked$Location==1, "Global North", "Global South")
write.csv(click_position_unclicked, file = "click_position_unclicked.csv")

# --------------------PART IV: Correlation exploring correlation between pop number and click number: weak but significant correlation with p smaller than .001
library("Hmisc")
ramp$Location_n <- ifelse(ramp$Location_n==1,0,1)
corr <- round(cor(ramp[,c(1, 4, 6,  8, 9, 10)], method = "spearman"), 3)

m12 <- glm(click_weighted ~ Location + position + device + Location*device*position, family = gaussian(link = "identity"), data = ramp)
summary(m12)
confint(m12)






country_click_device <- ramp%>%
  select(clicks, Country, pop2019, Location, device, click_weighted)%>%
  group_by(Country, Location, device)%>%
  summarise(click_s = sum(click_weighted), click = sum(clicks))%>%
  arrange(desc(click_s))
country_click_device$Location <- ifelse(country_click_device$Location==1, "Global North", "Global South")
write.csv(country_click_device, file = "country_click_device.csv")



top_10 <- top_10%>%
  mutate(rank = order(clicks_s))
rank(top_10$clicks_s)
rank(c(10,30,20,50,40))
##---bottom 15 countries which generate the fewest clicks
tail(ramp%>%
       select(click_weighted, Country, Location)%>%
       group_by(Country, Location)%>%
       summarise(clicks = sum(click_weighted))%>%
       arrange(desc(clicks)), 15)

country_click_device <- ramp%>%
  select(clicks, Country, pop2019, Location, device, click_weighted)%>%
  group_by(Country, Location, device)%>%
  summarise(click_s = sum(click_weighted), click = sum(clicks))%>%
  arrange(desc(click_s))
country_click_device$Location <- ifelse(country_click_device$Location==1, "Global North", "Global South")
write.csv(country_click_device, file = "country_click_device.csv")






#generate a table of clicks by region and device



ramp %>%
  select(clicks, device, Location)%>%
  group_by(Location, device)%>%
  summarise(clicksum = sum(clicks))%>%
  arrange(desc(clicksum))%>%
  mutate(percent = clicksum/sum(clicksum)*100)

region_device_click <- ramp %>%
  select(click_weighted, device, Location)%>%
  group_by(Location, device)%>%
  summarise(clicksum = sum(click_weighted))%>%
  arrange(desc(clicksum))%>%
  mutate(percent = clicksum/sum(clicksum)*100)
region_device_click$Location <- ifelse(region_device_click$Location=="1", "Global North", "Global South")
write.csv(region_device_click, file = "region_device_click.csv")
getwd()
setwd("/Users/minhpham/Desktop/RAMP August (cont)")

region_device_click_un <- describeBy(ramp$clicks, list(ramp$Location, ramp$device), mat=TRUE)
write.csv(region_device_click_un, file = "region_device_click_un.csv")






country_click <- ramp%>%
  select(clicks, Country, Location)%>%
  group_by(Country, Location)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  group_by(Location)%>%
  mutate(ave_click <- mean(clicks))

country_click_by_pop <- ramp%>%
  select(clicks, Country, Location, pop2019)%>%
  group_by(Country, Location, pop2019)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))%>%
  mutate(ave_click <- clicks/pop2019)%>%
  group_by(Location)%>%
  mutate(ave_click<-mean(`ave_click <- clicks/pop2019`))
country_click_by_pop$Location <- ifelse(country_click_by_pop$Location=="1", "Global North", "Global South")
write.csv(country_click_by_pop, file = "country_click_by_pop.csv")


country_click$Location <- ifelse(country_click$Location=="1", "Global North", "Global South")
write.csv(country_click, file = "country_click_un.csv")


str(clics_north_south_1)
str(dat6)
str(country_click)
dat6$Location <- as.factor(dat6$Location)
clicks_north_south_2 <- full_join(dat6, clics_north_south_1, by = "Location")
clicks_north_south_2%>%
  mutate(average_click <- )

clicks_north_south_2$Location <- ifelse(clicks_north_south_2$Location=="1", "Global North", "Global South")

write.csv(clicks_north_south_2, file = "clicks_north_south_2.csv")
getwd()
setwd("/Users/minhpham/Desktop/RAMP August(cont)")
head(dat6)


ramp%>%
  select(click_weighted,Location)%>%
  group_by(Location)%>%
  summarise(clicks = sum(click_weighted))%>%
  mutate(percent = clicks/sum(clicks)*100)



# country vs. clicks, aggregated
country_click <- ramp%>%
  select(clicks, Country, pop2019, Location)%>%
  group_by(Country, pop2019, Location)%>%
  summarise(clicks = sum(clicks))%>%
  arrange(desc(clicks))

country_click$click_weighted <- (country_click$clicks/country_click$pop2019)*100 # total number of clicks per 100 people 
country_click$Location <- ifelse(country_click$Location=="1", "Global North", "Global South")
write.csv(country_click, file = "country_click.csv")





country_click%>%
  select(Location, pop2019)%>%
  group_by(Location)%>%
  summarise(pop = sum(pop2019))%>%
  mutate(percent = pop/sum(pop)*100)

head(country_click)


##---top 15 countries which generate most clicks: non-standardized
top_10 <- head(ramp%>%
                 select(clicks, Country, pop2019, Location, click_weighted)%>%
                 group_by(Country, pop2019, Location)%>%
                 summarise(clicks_s = sum(click_weighted), clicks = sum(clicks))%>%
                 arrange(desc(clicks_s)),10)


top_10 <- head(ramp%>%
       select(clicks, Country, pop2019, Location, click_weighted)%>%
       group_by(Country, pop2019, Location)%>%
       summarise(clicks_s = sum(click_weighted), clicks = sum(clicks))%>%
  arrange(desc(clicks_s)),10)
top_10$Location <- ifelse(top_10$Location=="1", "Global North", "Global South")
top_10 <- top_10%>%
  mutate(rank = order(clicks_s))
rank(top_10$clicks_s)
rank(c(10,30,20,50,40))
write.csv(top_10, file = "top_10.csv")









head(ramp)

##----explore clicks in global north
head(country_click%>%
       filter(Location=="1")%>%
       arrange(desc(click_weighted)), 10)

tail(country_click%>%
       filter(Location=="1")%>%
       arrange(desc(click_weighted)), 10)

#e----xplore clicks in global south
head(country_click%>%
       filter(Location=="0")%>%
       arrange(desc(click_weighted)), 10)

tail(country_click%>%
       filter(Location=="0")%>%
       arrange(desc(click_weighted)), 10)


## position vs. clicks table
library(psych)
describeBy(ramp$clicks, list(ramp$Location, ramp$device), mat=TRUE)
describeBy(ramp$click_weighted, list(ramp$Location, ramp$device), mat=TRUE)
describeBy(ramp$position, list(ramp$Location, ramp$device), mat=TRUE)



## the above includes all rows with 0 clicks
## useful info but lets set a boolean for clicks > 0
ramp_clicked <- ramp %>% filter(clicks > 0)
describeBy(ramp_clicked$clicks, list(ramp_clicked$Location, ramp_clicked$device), mat=TRUE)
describeBy(ramp_clicked$click_weighted, list(ramp_clicked$Location, ramp_clicked$device), mat=TRUE)
describeBy(ramp_clicked$position, list(ramp_clicked$Location, ramp_clicked$device), mat=TRUE)
describeBy(ramp_clicked$position, list(ramp_clicked$device), mat=TRUE)


ramp_unclicked <- ramp %>% filter(clicks == 0)

describeBy(ramp_unclicked$position, list(ramp_unclicked$Location, ramp_unclicked$device), mat=TRUE)
describeBy(ramp_clicked$position, list(ramp_clicked$Location, ramp_clicked$device), mat=TRUE)
describeBy(ramp_unclicked$position, list(ramp_unclicked$device), mat=TRUE)
describeBy(ramp_clicked$position, list(ramp_clicked$device), mat=TRUE)

##-- position, click vs. location, clicked
ramp_clicked_south <- ramp%>%
  filter(Location==0, clicks >0)

describeBy(ramp_clicked_south$position, list(ramp_clicked_south$device), mat=TRUE)

##-- position, click vs. location, clicked
ramp_clicked_north <- ramp%>%
  filter(Location==1, clicks >0)

describeBy(ramp_clicked_north$position, list(ramp_clicked_north$device), mat=TRUE)

##-- position, click vs. location, unclicked
ramp_unclicked_south <- ramp%>%
  filter(Location==0, clicks ==0)

describeBy(ramp_unclicked_south$position, list(ramp_unclicked_south$device), mat=TRUE)

##-- position, click vs. location, unclicked
ramp_unclicked_north <- ramp%>%
  filter(Location==1, clicks ==0)

describeBy(ramp_unclicked_north$position, list(ramp_unclicked_north$device), mat=TRUE)


# exploring correlation between pop number and click number: weak but significant correlation with p smaller than .001
library("Hmisc")
str(ramp)
head(ramp)
ramp$Location_n <- ifelse(ramp$Location_n==1,0,1)

cor(ramp$position, ramp$clicks, method = c("spearman"))






m1 <- lm(clicks~Location, data = country_click)
summary(m1)

m2 <- lm(click_weighted~Location, data = country_click)
summary(m2)
confint(m2)
anova(m2, test = "Chisq")

#finding correlation
head(ramp)
corr <- round(cor(ramp[,c(1, 4, 6,  8, 9, 10)]), 1)
library(ggcorrplot)
ggcorrplot(corr, 
           method = "circle",
           hc.order = TRUE,
           type = "upper",
           outline.color = "white")


hist(ramp$click_weighted, xlim = c(0, .08), col = "grey",
     xlab = "click")
range(ramp$click_weighted)

m3 <- lm(click_weighted ~ Location, data = ramp)
summary(m3)
confint(m3)

m4 <- glm(click_weighted ~ Location, family = gaussian(link = "identity"), data = ramp)
summary(m4)
confint(m4)

m5 <- glm(click_weighted ~ Location, family = gaussian(link = "log"), data = ramp)
summary(m5)
confint(m4)

library(gamlss)
m6 <- gamlss(click_weighted ~ Location, family = NO, data = ramp, trace = FALSE)
summary(m6)


# estimated standard deviation of normal error term
fitted(m6, "sigma")[1]

#---fit normal distribution
m7 <- gamlss(click_weighted ~ Location, 
             sigma.fo = ~ Location,
             family = NO, data = ramp, trace = FALSE)
summary(m7)

#--fit gamma distribution
m8 <- gamlss(click_weighted ~ Location, 
             sigma.fo = ~ Location,
             family = GA, data = ramp, trace = FALSE)

summary(m8)
#---fit inverse Gaussian distribution
m9 <- gamlss(click_weighted ~ Location, 
             sigma.fo = ~ Location,
             family = IG, data = ramp, trace = FALSE)
summary(m9)

#----check model fits
GAIC(m6, m7, m8, m9)

m10 <- gamlss(click_weighted ~ Location + device, 
              sigma.fo = ~ Location + device,
              family = IG, data = ramp, trace = FALSE)
summary(m10)
str(ramp)
head(ramp)

m11 <- glm(click_weighted ~ Location + position + device + Location*device, family = gaussian(link = "identity"), data = ramp)
summary(m11)

m12 <- glm(click_weighted ~ Location + position + device + Location*device*position, family = gaussian(link = "identity"), data = ramp)
summary(m12)
cor(ramp$position, ramp$clicks, method = c("spearman"))
cor(ramp$position, ramp$clicks, method = c("pearson"))
cor(ramp$position, ramp$clicks, method = c("kendall"))


names(ramp)
head(ramp)

describeBy(ramp_clicked$clicks, list(ramp_clicked$Location, ramp_clicked$clicked, ramp_clicked$device), mat=TRUE)
describeBy(ramp_clicked$position, list(ramp_clicked$Location, ramp_clicked$clicked, ramp_clicked$device), mat=TRUE)




