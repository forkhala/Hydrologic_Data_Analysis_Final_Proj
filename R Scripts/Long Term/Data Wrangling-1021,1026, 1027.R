


########## Section I - USGS Site Selection - Long Term ##########

# Packages required for this section
library(dataRetrieval)
library(tidyverse)

##### HUC 1021 #####
# Generate a series of 8-digit HUC no
huc1021all.site.no <- seq(from = 10210001, to = 10210010, by = 1)

# Get all sites within 1021
huc1021.sites <- whatNWISdata(huc = huc1021all.site.no) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
  # I picked and rearranged columns I think relevant to our project; we may change it if we want.

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1021.sites.N <- huc1021.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
  # Last line appends "_N" to all column names, except for the first 6, because we need to join by the
  # first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1021.sites.P <- huc1021.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1021.sites.D <- huc1021.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1021.sites.DNP <- huc1021.sites.D %>%
  inner_join(., huc1021.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1021.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
  # count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
  # them for different HUC4 region.
View(huc1021.sites.DNP)

# Record site nos. of selected ones
huc1021.site.no <- unique(huc1021.sites.DNP$site_no)
huc1021.site.no

# Select best sites
best1021.site <- huc1021.sites.DNP %>% 
  filter_at(vars(starts_with("end_date")), all_vars(. > "2010-01-01"))
View(best1021.site)
best1021.site.no <- c("06775900", "06794000")

#---- HUC1021 end ----


##### HUC 1026 #####

huc1026all.site.no1 <- seq(from = 10260001, to = 10260010, by = 1)
huc1026all.site.no2 <- seq(from = 10260010, to = 10260015, by = 1)


# Get all sites within 1021
huc1026.sites1 <- whatNWISdata(huc = huc1026all.site.no1) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1026.sites2 <- whatNWISdata(huc = huc1026all.site.no2) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1026.sites <- rbind(huc1026.sites1, huc1026.sites2)

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1026.sites.N <- huc1026.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1026.sites.P <- huc1026.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1026.sites.D <- huc1026.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))


# Find out the sites that are common in D N P data
huc1026.sites.DNP <- huc1026.sites.D %>%
  inner_join(., huc1026.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1026.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50) %>%
  filter_at(vars(starts_with("end_date")), any_vars(. > "2010-01-01"))
View(huc1026.sites.DNP)

# Record site nos. of selected ones
huc1026.site.no <- unique(huc1026.sites.DNP$site_no)
huc1026.site.no

# Select best site
best1026.site.no <- c("06877600", "06874000")

#---- 1026 end ----

##### 1027 #####

huc1027all.site.no1 <- seq(from = 10270101, to = 10270104, by = 1)
huc1027all.site.no2 <- seq(from = 10270201, to = 10270207, by = 1)

# Get all sites within 1021
huc1027.sites1 <- whatNWISdata(huc = huc1027all.site.no1) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1027.sites2 <- whatNWISdata(huc = huc1027all.site.no2) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
huc1027.sites <- rbind(huc1027.sites1, huc1027.sites2)

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1027.sites.N <- huc1027.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1027.sites.P <- huc1027.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1027.sites.D <- huc1027.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Find out the sites that are common in D N P data
huc1027.sites.DNP <- huc1027.sites.D %>%
  inner_join(., huc1027.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1027.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
View(huc1027.sites.DNP)

# Record site nos. of selected ones
huc1027.site.no <- unique(huc1027.sites.DNP$site_no)
huc1027.site.no

# Select best sites
best1027.site <- huc1027.sites.DNP %>%
  filter_at(vars(starts_with("end_date")), all_vars(. > "2010-01-01"))
View(best1027.site)

best1027.site.no <- c("06892350", "06887500")
#---- 1027 end ----

bestsites1021.1026.1027 <- c("06775900", "06794000", "06877600", "06874000", "06892350", "06887500")
