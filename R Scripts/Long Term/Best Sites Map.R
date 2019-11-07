pacman::p_load(tidyverse, dataRetrieval, sf, maps)

bestsites1021.1026.1027.1030 <- 
  c("06775900", "06794000", "06877600", "06874000", "06892350", 
    "06887500", "06934500", "06894000")
bestsites1024.1025 <- c("06844500", "06856600", "06818000", "06810000")
bestsites1020.1023 <- c("06768000", "06805500", "06775900", "06794000", 
                        "06800000", "06800500", "06609500", "06610000")
bestsites1028.1029 <- c("06902000", "06905500", "06921070", "06926510")

best.sites <- c(bestsites1020.1023, bestsites1021.1026.1027.1030, 
                bestsites1024.1025, bestsites1028.1029)
best.sites <- unique(best.sites)

best.sites.info <- whatNWISdata(sites=best.sites) 

best.sites.lat.long <- best.sites.info %>%
  group_by(site_no) %>%
  summarise(Lat = mean(dec_lat_va), Long=mean(dec_long_va))

#creating spatial dataframe and map
best.sites.spatial <- st_as_sf(best.sites.lat.long,
                                   coords=c("Long", "Lat"), crs = 4269)
proj <- st_crs(best.sites.spatial) #lat and long are in 4269

states <- st_as_sf(map(database = "state", plot = TRUE, fill = TRUE, col = "white"))
states.map <- states %>% filter(ID =="nebraska" | ID=="kansas" | ID=="missouri" 
                                | ID=="iowa")
states.map <- st_set_crs(states.map, proj) #transform to match spatial datafram
st_crs(states.map)

best.sites.map <- ggplot() +
  geom_sf(data = states.map, fill = "white") +
  geom_sf(data = best.sites.spatial,  
          alpha = 0.5, size = 1)
print(best.sites.map)

waterfeatures <- st_read("./Data/Shapefiles/CopyOfhydrogl020.dbf")
waterfeatures <- waterfeatures %>% filter(STATE == "MO" | STATE == "NE" | 
                                            STATE == "IA" | STATE == "KS")
waterfeatures <- filter(waterfeatures, FEATURE != "Apparent Limit" & FEATURE != "Closure Line")

Waterfeaturesplot <- 
  ggplot(waterfeatures) +
  geom_sf(aes(fill = FEATURE, color = FEATURE)) +
  scale_color_viridis_d(option = "magma", end = 0.9) +
  scale_fill_viridis_d(option = "magma", end = 0.9)
print(Waterfeaturesplot)
waterfeatures <- st_set_crs(waterfeatures, 4269)

best.sites.map <- ggplot() +
  geom_sf(data = states.map, fill = "white") +
  geom_sf(data = waterfeatures, size = 0.4) +
  geom_sf(data = best.sites.spatial, fill="magenta", color="magenta", 
          alpha = 0.5, size = 2)
print(best.sites.map)
