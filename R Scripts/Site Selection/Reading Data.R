getwd()
library(tidyverse)
library(dataRetrieval)

#reading in best site information for the best sites in our hucs

#best site information for each huc
bestsites1021.1026.1027.1030 <- 
  c("06775900", "06794000", "06877600", "06874000", "06892350", 
    "06887500", "06934500", "06894000")
bestsites1024.1025 <- c("06844500", "06856600", "06818000", "06810000")
bestsites1020.1023 <- c("06768000", "06805500", "06775900", "06794000", 
                        "06800000", "06800500", "06609500", "06610000")
bestsites1028.1029 <- c("06902000", "06905500", "06921070", "06926510")

best.sites <- c(bestsites1021.1026.1027.1030, bestsites1024.1025, 
                bestsites1020.1023, bestsites1028.1029)
best.sites <- unique(best.sites)

#reading in daily discharge values for best sites in each huc
bestsites.discharge <- readNWISdv(siteNumbers = c(best.sites),
                                       parameterCd = "00060", #discharge
                                       startDate = "",
                                       endDate = "")
names(bestsites.discharge)[4:5] <- c("Discharge", "Approval Code")

bestsites.discharge$parm_cd <- "00060"

#Nitrogen and Phosphorous for best sites in each
bestsites.NP <- readNWISqw(siteNumbers = c(best.sites),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")

names(bestsites.NP)[3] <- c("Date")


#joinining datatables for water quality and discharge data
bestsites.DNP <- full_join(bestsites.discharge, bestsites.NP,
                       by = c("site_no", "agency_cd", "Date"))

bestsites.DNP <- bestsites.DNP %>%
  mutate(parameter = case_when(parm_cd == "00060" ~ "Discharge",
                               parm_cd == "00600" ~ "Total Nitrogen",
                               parm_cd == "00665" ~ "Total Phosphorus"))

#structure of daily value dataframe
dailyvalue.summary <- summary(bestsites.DNP)

#summary of data structure
kable(dailyvalue.summary, 
      caption = "Summary of Daily Value Data in the
      Missouri River Basin") %>% 
  kable_styling(latex_options = c("hold_position", "striped", 
                                  "scale_down")) %>% 
  kableExtra::landscape() 

#converting bestsites.DNP file to .csv
write.csv(bestsites.DNP, "./Data/Raw/bestsites.DNP.csv")

## ---High Frequency Data----

#reading in high frequency data for all sites

highfreq1024.1025 <-  c("06808500", "06817000")
highfreq1028.1029 <- c("06902000")
highfreq1020.1023 <-  c("06604440")
highfreq.1027.1030 <- c("06892350", "06892513", "06934500")

highfreqsites <- c(highfreq1024.1025, highfreq1028.1029, highfreq1020.1023,
                   highfreq.1027.1030)

#checking to make sure there are no duplicate sites
highfreqsites <- unique(highfreqsites)

#reading in data for high frequency N and D

highfreqsiteinfo <- whatNWISdata(siteNumbers = highfreqsites)
highfreqsiteinfo <- highfreqsiteinfo %>% filter(parm_cd =="99133" & data_type_cd=="uv")

#writing .csv file
write.csv(highfreqsiteinfo, "./Data/Raw/highfreqsiteinfo.csv")



