pacman::p_load(tidyverse, dataRetrieval, sf, maps)
theme_set(theme_classic())
##### Map sites selected, water bodies, and watersheds in the study region ####
# Gathering site nos from scripts
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
# Generate a list of selected sites with info on huc and names
site.list <- whatNWISdata(siteNumbers = best.sites, parameterCd = "00060") %>%
  select(site_no, station_nm, huc_cd) %>%
  group_by(site_no) %>%
  summarise(site_nm = first(station_nm),
            huc_cd = first(huc_cd)) %>%
  arrange(huc_cd) %>%
  mutate(huc4 = substr(huc_cd, start = 1, stop = 4))

huc4_nm <- rep(c("Platte", "Loup", "Elkhorn", "Missouri-Little Sioux", "Missouri-Nishnabotna",
                 "Republican", "Smoky Hill", "Kansas", "Chariton-Grand", "Gasconade-Osage",
                 "Lower Missouri"), each = 2)
site.list <- cbind(site.list, huc4_nm)
# save the list
write.csv(site.list, file="./Data/Processed/bestsiteslists.csv")



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
  geom_sf(data = waterfeatures, size = 0.4, color="lightgrey") +
  geom_sf(data = best.sites.spatial, fill="magenta", color="magenta", 
          alpha = 0.5, size = 2)
print(best.sites.map)

###### Using Watershed (HUC4) Boundary #####
# Still need run most codes above, except waterfeature ones (starting around line 44)
# and those producing graphs

# import watershed shapefile
AllHUC4 <- st_read("./Data/Shapefiles/WBD_10_HU2_Shape/WBDHU4.dbf")
HUC4.SE <- AllHUC4 %>%
  filter(HUC4 %in% seq(from = 1020, to = 1030, by = 1))
HUC4.NW <- AllHUC4 %>%
  filter(HUC4 %in% seq(from = 1000, to = 1019, by = 1))

# import stream geometric file (NOTE shapefiles not included in repos)
st_layers("../Untracked Proj Data/Small_Scale_Map/hydr48m010g.gdb")
streams <- st_read("../Untracked Proj Data/Small_Scale_Map/hydr48m010g.gdb", layer = "Stream")%>%
  st_zm(drop = T, what = "ZM") # drop z/m dimension, and only keep x, y dimensions for 2D figures

streams.HU10 <- streams %>%
  filter(Region == 10)

missouri <- streams.HU10 %>%
  filter(Name == "Missouri River")

# all states related to missouri region
allstates.map <- states %>% 
  filter(ID %in% c("montana","north dakota","south dakota","nebraska","iowa","kansas","missouri",
                   "wyoming","colorado", "minnesota", "idaho"))
allstates.map <- st_set_crs(allstates.map, proj) #set projection

sitemap <- ggplot() +
  geom_sf(data = allstates.map, fill = "white", size = 0.4) +
  geom_sf(data = HUC4.SE, aes(fill = Name), alpha = 0.5) +
  geom_sf(data = HUC4.NW, color = "gray30", alpha = 0.5) +
  geom_sf(data = streams.HU10, color = "dodgerblue", alpha = 0.5, size = 0.10) +
  geom_sf(data = missouri, color = "blue", alpha = 0.8, size = 0.5) +
  scale_fill_brewer(palette = "Paired") +
  geom_sf(data = best.sites.spatial, fill="red", color="red", 
          alpha = 0.8, size = 1) +
  theme(legend.margin = margin(0,0,0,0, "pt"), legend.text = element_text(size = 7.5), 
        legend.title = element_text(size = 8.5)) + 
  labs(fill = "Watershed Name")

# Caution this takes time to display, and even longer than ggsave()
# print(sitemap)
# save file
ggsave("site_map.jpg", sitemap, dpi = 300, width = 9, height = 9, units = "in")

#---- site mapping ends ----


#look for pH
best.sites.pH <- best.sites.info %>%
  filter(parm_cd=="00400")
unique(best.sites.pH$site_no)

#look for total coliform
best.sites.tc <- best.sites.info %>%
  filter(parm_cd=="31501")
unique(best.sites.tc$site_no)
