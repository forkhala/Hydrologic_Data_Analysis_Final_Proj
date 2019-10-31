########## Section I - USGS Site Selection ##########

# Packages required for this section
library(dataRetrieval)
library(tidyverse)

##### HUC 1020 #####
# Generate a series of 8-digit HUC no
huc1020pat1.site.no <- seq(from = 10200101, to = 10200103, by = 1)
huc1020pat2.site.no <- seq(from = 10200201, to = 10200203, by = 1)
huc1020all.site.no <- c(huc1020pat1.site.no,huc1020pat2.site.no)

# Get all sites within 1020
huc1020.sites <- whatNWISdata(huc = huc1020all.site.no) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
# I picked and rearranged columns I think relevant to our project; we may change it if we want.

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1020.sites.N <- huc1020.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1020.sites.P <- huc1020.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1020.sites.D <- huc1020.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1020.sites.DNP <- huc1020.sites.D %>%
  inner_join(., huc1020.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1020.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1020.sites.DNP)

# Record site nos. of selected ones
huc1020.site.no <- unique(huc1020.sites.DNP$site_no)
huc1020.site.no #4 sites

#---- HUC1020 end ----

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
huc1021.site.no #4 sites

#---- HUC1021 end ----

##### HUC 1022 #####
# Generate a series of 8-digit HUC no
huc1022all.site.no <- seq(from = 10220001, to = 10220004, by = 1)

# Get all sites within 1022
huc1022.sites <- whatNWISdata(huc = huc1022all.site.no) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
# I picked and rearranged columns I think relevant to our project; we may change it if we want.

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1022.sites.N <- huc1022.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1022.sites.P <- huc1022.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1022.sites.D <- huc1022.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1022.sites.DNP <- huc1022.sites.D %>%
  inner_join(., huc1022.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1022.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1022.sites.DNP)

# Record site nos. of selected ones
huc1022.site.no <- unique(huc1022.sites.DNP$site_no)
huc1022.site.no #4 sites

#---- HUC1022 end ----

##### HUC 1023 #####
# Generate a series of 8-digit HUC no
huc1023all.site.no <- seq(from = 10230001, to = 10230007, by = 1)

# Get all sites within 1023
huc1023.sites <- whatNWISdata(huc = huc1023all.site.no) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)
# I picked and rearranged columns I think relevant to our project; we may change it if we want.

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1023.sites.N <- huc1023.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1023.sites.P <- huc1023.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1023.sites.D <- huc1023.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1023.sites.DNP <- huc1023.sites.D %>%
  inner_join(., huc1023.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1023.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1023.sites.DNP)

# Record site nos. of selected ones
huc1023.site.no <- unique(huc1023.sites.DNP$site_no)
huc1023.site.no #4 sites

#---- HUC1023 end ----

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
  filter(parm_cd == "00600") %>%
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
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
View(huc1026.sites.DNP)

# Record site nos. of selected ones
huc1026.site.no <- unique(huc1026.sites.DNP$site_no)
huc1026.site.no

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

#---- 1027 end ----

#getting data for huc 1028
#creating seq for grand and chariton (two different sets of huc numbers)
grand.huc = seq(10280101, 10280103, by = 1)
chariton.huc = seq(10280201, 10280203, by = 1)

#combining vectors together to be in one vector
huc1028all.site.no <- c(grand.huc, chariton.huc)

#### get all sites within 1028 ####
huc1028.sites <- whatNWISdata(huc = huc1028all.site.no) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1028.sites.N <- huc1028.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1028.sites.P <- huc1028.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1028.sites.D <- huc1028.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1028.sites.DNP <- huc1028.sites.D %>%
  inner_join(., huc1028.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1028.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1028.sites.DNP)

# Record site nos. of selected ones
huc1028.site.no <- unique(huc1028.sites.DNP$site_no)
huc1028.site.no #11 sites

#--- 1028 end ----

#### HUC 1029 Data ####

#getting data for huc 1029
#creating seq for osage and gasconade (two different sets of huc numbers)
osage.huc = seq(10290101, 10290110, by = 1)
osage.huc111 = 10290111
gasconade.huc = seq(10290201, 10290203, by = 1)

#get all sites within 1029 osage (until huc 110)
huc1029.osage.sites <- whatNWISdata(huc = osage.huc) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)

huc1029.osage.huc111 <- whatNWISdata(huc = osage.huc111) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)

huc1029.gasconade.huc <- whatNWISdata(huc = gasconade.huc) %>%
  select(site_no, station_nm, huc_cd, site_tp_cd, dec_lat_va, dec_long_va,
         parm_cd, data_type_cd, stat_cd, srs_id, begin_date, end_date, count_nu)

#combining all datasets into one dataframe
huc1029.all.sites <- rbind(huc1029.osage.sites, huc1029.osage.huc111, huc1029.gasconade.huc)

# Filter for sites with total nitrogen "00600"
# (Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, mg/l)
huc1029.sites.N <- huc1029.all.sites %>%
  filter(parm_cd == "00600") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))
# Last line appends "_N" to all column names, except for the first 6, because we need to join by the
# first 6 cols later.

# Sites with total phosphorus "00665"
# (Phosphorus, water, unfiltered, mg/l as phosphorus)
huc1029.sites.P <- huc1029.all.sites %>%
  filter(parm_cd == "00665") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'P'))

# Sites with discharge "00060"
# (Discharge, cubic feet per second) there is also discharge in metric unit, but few sites have it.
huc1029.sites.D <- huc1029.all.sites %>%
  filter(parm_cd == "00060")%>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'D'))

# Select and join sites with all N, P, Discharge data present. Dataframes are joined by first 6 columns
# so that duplicates of these basic info on sites will not be created during joining.

# (I used inner_join because we want to "return all rows from x where there are matching values 
# in y" (the help page). We can discuss which join function to be used during meeting.)
huc1029.sites.DNP <- huc1029.sites.D %>%
  inner_join(., huc1029.sites.N, 
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  inner_join(., huc1029.sites.P,
             by = c("site_no","station_nm","huc_cd","site_tp_cd","dec_lat_va","dec_long_va"))%>%
  filter(count_nu_D > 100 & count_nu_N > 50 & count_nu_P > 50)
# count_nu for count number of that variable; I arbitrarily chose these criteria. We can play with
# them for different HUC4 region.
View(huc1029.sites.DNP)

# Record site nos. of selected ones
huc1029.site.no <- unique(huc1029.sites.DNP$site_no)
huc1029.site.no #10 sites

#----1029 end ----

allsites <- rbind(huc1020.sites.DNP, huc1021.sites.DNP, huc1022.sites.DNP, huc1023.sites.DNP,
                  huc1024.sites.DNP, huc1025.sites.DNP, huc1026.sites.DNP, huc1027.sites.DNP,
                  huc1028.sites.DNP, huc1029.sites.DNP)
allsiteno <- unique(allsites$site_no)
allsiteno
