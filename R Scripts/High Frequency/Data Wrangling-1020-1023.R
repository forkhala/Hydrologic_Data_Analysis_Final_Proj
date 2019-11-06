########## Section I - USGS Site Selection - High Frequency ##########

# Packages required for this section
library(dataRetrieval)
library(tidyverse)

##### HUC 1020 #####
# Generate a series of 8-digit HUC no
huc1020pat1.site.no <- seq(from = 10200101, to = 10200103, by = 1)
huc1020pat2.site.no <- seq(from = 10200201, to = 10200203, by = 1)
huc1020all.site.no <- c(huc1020pat1.site.no,huc1020pat2.site.no)

# Get all sites within 1021
huc1020.sites <- whatNWISdata(huc = huc1020all.site.no, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
# I picked and rearranged columns I think relevant to our project; we may change it if we want.

# Filter for sites with total nitrogen "99133"
# (Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen)
huc1020.sites.N <- huc1020.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1020.sites.D <- huc1020.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1020.sites.DN <- huc1020.sites.D %>%
  inner_join(., huc1020.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1020.sites.DN)

# Record site nos. of selected ones
# No sites have high freq N measurements

#---- HUC1020 end ----

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

##### HUC 1022 #####

huc1022all.site.no <- seq(from = 10220001, to = 10220004, by = 1)

# Get all sites within 1022
huc1022.sites <- whatNWISdata(huc = huc1022all.site.no, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)

# Filter for sites with total nitrogen "99133"
# (Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen)
huc1022.sites.N <- huc1022.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1022.sites.D <- huc1022.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))


# Find out the sites that are common in D N P data
huc1022.sites.DN <- huc1022.sites.D %>%
  inner_join(., huc1022.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))
View(huc1022.sites.DN)

# Record site nos. of selected ones
# No sites have high freq N sites

#---- 1022 end ----

##### 1023 #####

huc1023all.site.no <- seq(from = 10230001, to = 10230007, by = 1)

# Get all sites within 1023
huc1023.sites <- whatNWISdata(huc = huc1023all.site.no, service = "uv") %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)

# Filter for sites with total nitrogen "99133"
# (Nitrate plus nitrite, water, in situ, milligrams per liter as nitrogen)
huc1023.sites.N <- huc1023.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1023.sites.D <- huc1023.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Find out the sites that are common in D N P data
huc1023.sites.DN <- huc1023.sites.D %>%
  inner_join(., huc1023.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))
View(huc1023.sites.DN)

# Record site nos. of selected one
huc1023.site.no <- unique(huc1023.sites.DN$site_no)
huc1023.site.no # "06604440"

#---- 1023 end ----

highfreq1020.1023 <-  c("06604440")
