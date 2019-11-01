########## Section I - USGS Site Selection ##########

# Packages required for this section
library(dataRetrieval)
library(tidyverse)

##### HUC 1024 #####
# Generate a series of 8-digit HUC no
huc1024first10 <- seq(from = 10240001, to = 10240010, by = 1)
huc1024remaining <- seq(from = 10240011, to = 10240013, by = 1)

# Get all sites within 1024
huc1024.sites.first10 <- whatNWISdata(huc = huc1024first10) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1024.sites.remaining <- whatNWISdata(huc = huc1024remaining) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
#join the two datasets to have all HUC1024 subbasins
huc1024.sites.all <- rbind(huc1024.sites.first10,huc1024.sites.remaining)

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1024.sites.N <- huc1024.sites.all %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1024.sites.P <- huc1024.sites.all %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1024.sites.D <- huc1024.sites.all %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1024.sites.DNP <- huc1024.sites.D %>%
  inner_join(., huc1024.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1024.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1024.sites.DNP)

# Record site nos. of selected ones
huc1024.site.no <- unique(huc1024.sites.DNP$site_no)
huc1024.site.no #6 sites


#sites: 	06808500 and 06817000 have high freq uv for N; do they also for D?
# Sites with discharge "00060"
huc1024.sites.Duv <- huc1024.sites.all %>%
  filter(site_no== "06808500" | site_no== "06817000", parm_cd == "00060", data_type_cd=="uv")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))
#Yes, the two sites that have high freq N also have high freq D!

#---- HUC1024 end ----


##### HUC 1025 #####
# Generate a series of 8-digit HUC no
huc1025first10 <- seq(from = 10250001, to = 10250010, by = 1)
huc1025remaining <- seq(from = 10250011, to = 10250017, by = 1)

# Get all sites within 1025
huc1025.sites.first10 <- whatNWISdata(huc = huc1025first10) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1025.sites.remaining <- whatNWISdata(huc = huc1025remaining) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
#join the two datasets to have all HUC1025 subbasins
huc1025.sites.all <- rbind(huc1025.sites.first10,huc1025.sites.remaining)

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1025.sites.N <- huc1025.sites.all %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1025.sites.P <- huc1025.sites.all %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P')) 

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1025.sites.D <- huc1025.sites.all %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1025.sites.DNP <- huc1025.sites.D %>%
  inner_join(., huc1025.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1025.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1025.sites.DNP)

# Record site nos. of selected ones
huc1025.site.no <- unique(huc1025.sites.DNP$site_no)
huc1025.site.no #4 sites


#---- HUC1025 end ----

