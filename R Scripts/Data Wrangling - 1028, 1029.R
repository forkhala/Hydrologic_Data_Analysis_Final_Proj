getwd()
library(dataRetrieval)
library(tidyverse)

##### HUC 1028 #####
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

#filter for sites with instantaneous N data 
instantaneous.n.huc1028 <- huc1028.sites %>%
  filter(parm_cd == "99133") %>%
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N'))

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

#---- HUC1028 end ----

#looking at only daily values
huc1028.sites.dv <- huc1028.sites.DNP %>%
  filter(data_type_cd_D == "dv")

#seeing how many unique sites are in the daily value table
unique.dv.1028 <- unique(huc1028.sites.dv$site_no)
unique.dv.1028 #11 unique sites with DV data

#getting site numbers for the data
huc1028.sites.dv$site_no[8] #06902000; good longterm discharge, N, and P data
huc1028.sites.dv$site_no[10] #06905500; good longterm discharge, some N and P data

#discharge data for both sites in huc1028
huc1028.sites.discharge <- readNWISdv(siteNumbers = c("06902000",
                                                      "06905500"),
                                      parameterCd = c("00060"), #discharge
                                      startDate = "",
                                      endDate = "")
names(huc1028.sites.discharge)[4:5] <- c("Discharge", "Approval Code")

#Nitrogen and Phosphorous for both sites in huc1028
huc1028.sites.NP <- readNWISqw(siteNumbers = c("06902000", 
                                               "06905500"),
                               parameterCd = c("00600", "00665"), #TN, TP
                               startDate = "",
                               endDate = "")

#join N&P dataset with discharge dataset? 

##START HERE!
#looking for high frequency nitrogen data in huc1028 sites
huc1028.sites.N.uv <- huc1028.sites %>%
  filter(parm_cd == "99133") %>% 
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N')) #one site (06902000) with this parameter code; from 2017 - 2018

#filtering huc1028 sites for just discharge uv values
huc1028.sites.discharge.uv <- huc1028.sites.DNP %>%
  filter(data_type_cd_D == "uv") #06902000 & 06905500 for uv data

#reading in uv N information for sites 06902000 and 06905500
huc1028.sites.uv <- readNWISqw(siteNumbers = c("06902000",
                                               "06905500"),
                               parameterCd = c("00060", 
                                               "00630"), #discharge, TN
                               startDate = "",
                               endDate = "")
#write.csv("/Data/Raw/)

## ---- HUC 1028 Chosen Sites End ----

##### HUC 1029 #####

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
  filter(parm_cd == "00600") %>% #00630 is another parameter code for high frequency nitrate
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

#---- HUC1029 end ----

#----HUC1029 Chosen Sites ----

#narrow down HUCs by daily value of discharge (and high frequency), date range (preference to ones that go to current years) - do some
#exploratory data analysis on the sites to see that there are no gaps in the data (longest date range)

#1. find high frequency nitrate data and see if those site have high frequency data; also make sure there is 
#high frequency discharge data for it too
#2. find daily discharge data that is over long term along with nitrogen and phosphorous data
#uv data for nitrogen we use 99133 and 00630
#once 2 sites are chosen, write as a .csv and save in raw data files

#looking at only daily values
huc1029.sites.dv <- huc1029.sites.DNP %>%
  filter(data_type_cd_D == "dv")

#seeing how many unique sites are in the daily value table
unique.dv.1029 <- unique(huc1029.sites.dv$site_no)
unique.dv.1029 #10 unique sites with DV data

#could use site 06926510 (long term N and P data and some discharge (but not that longterm)
#use site 06919500 for long term discharge daily values; also use 06923500 for long term daily discharge

huc1029.sites.dv$site_no[4] #06921070; good longterm discharge, N, and P data
huc1029.sites.dv$site_no[10] #06926510; good longterm discharge, some N and P data

#discharge data for both sites in huc1029
huc1029.sites.discharge <- readNWISdv(siteNumbers = c("06921070",
                                      "06926510"),
                                      parameterCd = c("00060"), #discharge
                                      startDate = "",
                                      endDate = "")
names(huc1029.sites.discharge)[4:5] <- c("Discharge", "Approval Code")

#Nitrogen and Phosphorous for both sites in huc1029
huc1029.sites.NP <- readNWISqw(siteNumbers = c("06921070",
                               "06926510"),
                               parameterCd = c("00600", "00665"), #TN, TP
                               startDate = "",
                               endDate = "")

#looking for high frequency nitrogen data in huc1029 sites
huc1029.sites.N.uv <- huc1029.all.sites %>%
  filter(parm_cd == "99133") %>% 
  rename_at(vars(-(1:6)), ~ paste(., sep = "_",'N')) #no sites with this parameter code

#filtering huc1029 sites for just discharge uv values
huc1029.sites.discharge.uv <- huc1029.sites.DNP %>%
  filter(data_type_cd_D == "uv")

#reading in uv discharge information for sites 06919500 and 06923250
huc1029.sites.uv <- readNWISuv(siteNumbers = c("06919500",
                                                "06923250"),
                                parameterCd = c("00060"), #discharge
                                startDate = "",
                                endDate = "")
names(huc1029.sites.uv)[4:5] <- c("Discharge", "Approval Code")

# ---- HUC 1029 Chosen Sites end ----

bestsites1028.1029 <- c("06902000", "06905500", "06921070", "06926510")

highfreq1028.1029 <- c("06902000", "06905500", "06919500", "06923250") #no uv N data in 1029 and only one in 1028

