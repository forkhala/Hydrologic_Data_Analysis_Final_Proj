


########## Section I - USGS Site Selection - High Frequency ##########

# Packages required for this section
library(dataRetrieval)
library(tidyverse)

##### HUC 1021 #####
# Generate a series of 8-digit HUC no
huc1021all.site.no <- seq(from = 10210001, to = 10210010, by = 1)

# Get all sites within 1021
huc1021.sites <- whatNWISdata(huc = huc1021all.site.no, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
  # I picked and rearranged columns I think relevant to our project; we may change it if we want.

# Filter for sites with total nitrogen "99133"
# (Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen)
huc1021.sites.N <- huc1021.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
  # Last line appends "_N" to all column names, except for the first 6, because we need to join by the
  # first 6 cols later.

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1021.sites.D <- huc1021.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1021.sites.DN <- huc1021.sites.D %>%
  inner_join(., huc1021.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))
  # count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
  # them for different HUC4 region.
View(huc1021.sites.DN)

# Record site nos. of selected ones
# No sites have high freq N measurements

#---- HUC1021 end ----


##### HUC 1026 #####

huc1026all.site.no1 <- seq(from = 10260001, to = 10260010, by = 1)
huc1026all.site.no2 <- seq(from = 10260010, to = 10260015, by = 1)


# Get all sites within 1021
huc1026.sites1 <- whatNWISdata(huc = huc1026all.site.no1, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1026.sites2 <- whatNWISdata(huc = huc1026all.site.no2, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1026.sites <- rbind(huc1026.sites1, huc1026.sites2)

# Filter for sites with total nitrogen "99133"
# (Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen)
huc1026.sites.N <- huc1026.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1026.sites.D <- huc1026.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))


# Find out the sites that are common in D N P data
huc1026.sites.DN <- huc1026.sites.D %>%
  inner_join(., huc1026.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))
View(huc1026.sites.DN)

# Record site nos. of selected ones
# No sites have high freq N sites

#---- 1026 end ----

##### 1027 #####

huc1027all.site.no1 <- seq(from = 10270101, to = 10270104, by = 1)
huc1027all.site.no2 <- seq(from = 10270201, to = 10270207, by = 1)

# Get all sites within 1021
huc1027.sites1 <- whatNWISdata(huc = huc1027all.site.no1, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1027.sites2 <- whatNWISdata(huc = huc1027all.site.no2, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1027.sites <- rbind(huc1027.sites1, huc1027.sites2)

# Filter for sites with total nitrogen "99133"
# (Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen)
huc1027.sites.N <- huc1027.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1027.sites.D <- huc1027.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Find out the sites that are common in D N P data
huc1027.sites.DN <- huc1027.sites.D %>%
  inner_join(., huc1027.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))
View(huc1027.sites.DN)

# Record site nos. of selected ones
huc1027.site.no <- unique(huc1027.sites.DN$site_no)
huc1027.site.no # "06892350" "06892513"

#---- 1027 end ----

##### HUC 1030 #####
# Generate a series of 8-digit HUC no
huc1030all.site.no <- c(seq(from = 10300101, to = 10300104, by = 1), "10300200")

# Get all sites within 1030
huc1030.sites <- whatNWISdata(huc = huc1030all.site.no, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
# I picked and rearranged columns I think relevant to our project; we may change it if we want.

# Filter for sites with total nitrogen "99133"
# (Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen)
huc1030.sites.N <- huc1030.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1030.sites.D <- huc1030.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1030.sites.DN <- huc1030.sites.D %>%
  inner_join(., huc1030.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1030.sites.DN)

# Record site nos. of selected ones
huc1030.site.no <- unique(huc1030.sites.DN$site_no)
huc1030.site.no # "06934500"

#---- HUC1030 end ----

##### Summary of sites selected for high frequency (uv) in 1021, 1026, 1027, 1030 #####

highfreq.1027.1030 <- c("06892350", "06892513", "06934500")
# two sites in 1027, one in 1030; in the order of 1027, 1030; none in 1021, 1026
