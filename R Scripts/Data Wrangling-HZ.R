


########## Section I - USGS Site Selection ##########

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

#---- HUC1021 end ----


