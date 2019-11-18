####counties and population####

library(noncensus); library(tidyverse); library(sf); library(dataRetrieval);
library(sp); library(lubridate); library(lme4); library(stats); library(MASS);
library(car); library(GGally)

#database with population by county
data("counties")
countypop <- counties %>% filter(state %in% c("KS", "NE", "MO", "IA"))
countypop$county_name <- gsub("St. Louis city", "St. Louis City County", countypop$county_name)

#creating maps by county with polygon data
KS.county <- st_as_sf(map(database = "county",'kansas', plot=TRUE, fill = TRUE, col = "white"))
KS.county$state <- "KS"
KS.county$ID <- gsub("kansas,", "", KS.county$ID)
KS.county$ID <- stringr::str_to_title(KS.county$ID) %>%
  paste0(" County") %>%
  as.factor()
colnames(KS.county)[2] <- "county_name"

NE.county <- st_as_sf(map(database = "county",'nebraska', plot=TRUE, fill = TRUE, col = "white"))
NE.county$state <- "NE"
NE.county$ID <- gsub("nebraska,", "", NE.county$ID)
NE.county$ID <- stringr::str_to_title(NE.county$ID) %>%
  paste0(" County") %>%
  as.factor()
colnames(NE.county)[2] <- "county_name"

IA.county <- st_as_sf(map(database = "county",'iowa', plot=TRUE, fill = TRUE, col = "white"))
IA.county$state <- "IA"
IA.county$ID <- gsub("iowa,", "", IA.county$ID)
IA.county$ID <- stringr::str_to_title(IA.county$ID) %>%
  paste0(" County") %>%
  as.factor()
colnames(IA.county)[2] <- "county_name"

MO.county <- st_as_sf(map(database = "county",'missouri', plot=TRUE, fill = TRUE, col = "white"))
MO.county$state <- "MO"
MO.county$ID <- gsub("missouri,", "", MO.county$ID)
MO.county$ID <- stringr::str_to_title(MO.county$ID) %>%
  paste0(" County") %>%
  as.factor()
colnames(MO.county)[2] <- "county_name"

all.counties <- rbind(KS.county, NE.county, IA.county, MO.county)
#add in the . and the ' and the lower case letters
all.counties$county_name <- gsub("St ", "St. ", all.counties$county_name)
all.counties$county_name <- gsub("Ste ", "Ste. ", all.counties$county_name)
all.counties$county_name <- gsub("Obrien", "O'Brien", all.counties$county_name)
all.counties$county_name <- gsub("De Kalb", "DeKalb", all.counties$county_name)
all.counties$county_name <- gsub("Mcpherson", "McPherson", all.counties$county_name)
all.counties$county_name <- gsub("Mcdonald", "McDonald", all.counties$county_name)

st_crs(all.counties)

#combining database with population data and database with geometry data
county.pop.spatial <- merge(all.counties, countypop)
st_crs(county.pop.spatial)

write.csv(county.pop.spatial, "./Data/Processed/countypop.spatial.csv")

#read in best sites with spatial
best.sites.list <- read.csv("./Data/Processed/bestsiteslist.csv", as.is=TRUE)
best.sites.list$site_no <- paste0("0", best.sites.list$site_no)
best.sites <- best.sites.list$site_no

best.sites.info <- whatNWISdata(sites=best.sites)

best.sites.lat.long <- best.sites.info %>%
  group_by(site_no) %>%
  summarise(Lat = mean(dec_lat_va), Long=mean(dec_long_va))

#creating spatial dataframe and map
best.sites.spatial <- st_as_sf(best.sites.lat.long,
                               coords=c("Long", "Lat"), crs = 4269)
st_crs(best.sites.spatial)

county.pop.spatial <- st_transform(county.pop.spatial, 4269)
st_crs(county.pop.spatial)

sites.county <- st_intersection(best.sites.spatial, county.pop.spatial)
length(unique(sites.county$county_name))

bestsites.countypop.info <- full_join(sites.county, best.sites.list)
bestsites.countypop.info <- bestsites.countypop.info %>%
  select(site_no, site_nm, huc_cd, huc4, huc4_nm, state, county_name, county_fips, 
         population, geometry, site_lab)

#full list with site no, site name, population and county info
write.csv(bestsites.countypop.info, file = "./Data/Processed/bestsites.countypop.info.csv")

bestsites.countypop.info <- read.csv("./Data/Processed/bestsites.countypop.info.csv", as.is=TRUE)

bestsites.WQ <- read.csv("./Data/Raw/bestsites.WQ.csv")
unique(bestsites.WQ$parm_cd)

#caroline's wrangling of water quality data. I took out total coliform
bestsites.WQ.skinny <- bestsites.WQ %>%
  dplyr::select(Site = site_no,
         Date = Date,
         Parameter = parm_cd,
         Value = result_va,
         Discharge = X_00060_00003) %>%
  group_by(Date, Parameter, Site) %>%
  summarize(Value = mean(Value),
            Discharge = mean(Discharge)) %>%
  spread(key = Parameter, value = Value) %>% 
  rename(pH = '400', total.coliform = '31501', 
         Discharge2 = '60', total.nitrogen = '600', 
         total.phosphorus = '665') %>%
  mutate(Year = year(Date)) %>%
  select(-total.coliform) 


#wrangle so they can be joined
bestsites.WQ.skinny$Site <- paste0("0", bestsites.WQ.skinny$Site)
bestsites.countypop.info <- bestsites.countypop.info %>% rename_at("site_no",~"Site")

#join WQ info with site, population, and county df
WQ.countypop.joined <- left_join(bestsites.WQ.skinny, bestsites.countypop.info, 
                                 by = "Site")

##prep for making lm
hist(log(WQ.countypop.joined$total.nitrogen)) 
#nitrogen should be logged so it is normally distributed
str(WQ.countypop.joined)
WQ.countypop.joined$Site <- as.factor(WQ.countypop.joined$Site)
WQ.countypop.joined$state <- as.factor(WQ.countypop.joined$state)
WQ.countypop.joined$county_name <- as.factor(WQ.countypop.joined$county_name)
WQ.countypop.joined$Date <- as.Date(WQ.countypop.joined$Date, format = "%y-%m-%d")

#lm1
mod1 <- lm(data=WQ.countypop.joined, log(total.nitrogen) ~ Year + population + 
             factor(huc4) + total.phosphorus + factor(state))
summary(mod1)
plot(mod1)
step.mod <- stepAIC(mod1) #review what this means (negative values??)
formula(step.mod)
avPlots(step.mod)

#lm2
WQ.countypop.joined$Year.c <- WQ.countypop.joined$Year - mean(WQ.countypop.joined$Year)
WQ.countypop.joined$population.c <- WQ.countypop.joined$popultion - mean(WQ.countypop.joined$population)

mod2 <- lm(data=WQ.countypop.joined, log(total.nitrogen) ~ Year + population + 
             factor(huc4))
summary(mod2)
#Year, population, and 10/11 huc4 regions are statistically significant

#lm2
mod3 <- lmer(data=WQ.countypop.joined, log(total.nitrogen) ~ Year + population + huc4 +
               (1|Site))
summary(mod3) #not working with site as a random effect. consider rescaling??
anova(mod3)
interaction.plot(total.nitrogen, Site, population)

#lm4
hist(log(WQ.countypop.joined$total.phosphorus))
hist(log(WQ.countypop.joined$Discharge))
mod4 <- lm(data=WQ.countypop.joined, total.nitrogen ~ Discharge)
#what do I do with discharge?

plot(WQ.countypop.joined$Discharge, WQ.countypop.joined$total.nitrogen)
plot(log(WQ.countypop.joined$total.nitrogen), log(WQ.countypop.joined$total.phosphorus))

#centering number makes them smaller
#divide people by 10 or 100 so that it is your effect for every 100 people or something
#leave site out, huc as random effect
