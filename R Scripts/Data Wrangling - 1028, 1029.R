getwd()
library(dataRetrieval)
library(tidyverse)

#getting data for huc 1028
#creating seq for grand and chariton (two different sets of huc numbers)
grand.huc = seq(10280101, 10280103, by = 1)
chariton.huc = seq(10280201, 10280203, by = 1)

#combining vectors together to be in one vector
huc1028all.site.no <- c(grand.huc, chariton.huc)

#get all sites within 1028
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


##HUC 1029 Data

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


