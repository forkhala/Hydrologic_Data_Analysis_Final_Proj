####counties and population####

library(noncensus); library(tidyverse); library(sf); library(dataRetrieval);
library(sp)

#database with population by county
data("counties")
countypop <- counties %>% filter(state %in% c("KS", "NE", "MO", "IA"))

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

#combining database with population data and database with geometry data
county.pop.spatial <- 
  left_join(countypop, all.counties, by=c("state", "county_name"))

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
proj <- st_crs(best.sites.spatial)

new_crs <- sp::sptransform(best.sites.spatial, crs(all.counties))
crs(all.counties)
