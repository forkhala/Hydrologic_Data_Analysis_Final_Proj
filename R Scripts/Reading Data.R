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

#reading in daily discharge values for best sites in each huc
#huc1020
bestsites.discharge.1020 <- readNWISdv(siteNumbers = c("06768000", 
                                                          "06805500", 
                                                          "06775900", 
                                                          "06794000", 
                                                          "06800000", 
                                                          "06800500"),
                                          parameterCd = c("00060"), #discharge
                                          startDate = "",
                                          endDate = "")
#renaming columns to understand what they show
names(bestsites.discharge.1020)[4:5] <- c("Discharge", "Approval Code")
                                          
#huc1021
bestsites.discharge.1021 <- readNWISdv(siteNumbers = c("06775900", "06794000"), 
                                          parameterCd = c("00060"), #discharge
                                          startDate = "",
                                          endDate = "") 
#renaming columns to understand what they show
names(bestsites.discharge.1021)[4:5] <- c("Discharge", "Approval Code")

#huc1023
bestsites.discharge.1023 <- readNWISdv(siteNumbers = c("06609500", "06610000"), 
                                       parameterCd = c("00060"), #discharge
                                       startDate = "",
                                       endDate = "") 

#renaming columns to understand what they show
names(bestsites.discharge.1023)[4:5] <- c("Discharge", "Approval Code")

#huc1024
bestsites.discharge.1024 <- readNWISdv(siteNumbers = c("06818000", "06810000"), 
                                    parameterCd = c("00060"), #discharge
                                    startDate = "",
                                    endDate = "")
#renaming columns to understand what they show
names(bestsites.discharge.1024)[4:5] <- c("Discharge", "Approval Code")

#huc1025
bestsites.discharge.1025 <- readNWISdv(siteNumbers = c("06844500", "06856600"), 
                                    parameterCd = c("00060"), #discharge
                                    startDate = "",
                                    endDate = "")

#renaming columns to understand what they show
names(bestsites.discharge.1025)[4:5] <- c("Discharge", "Approval Code")

#huc1026
bestsites.discharge.1026 <- readNWISdv(siteNumbers = c("06877600", "06874000"), 
                                       parameterCd = c("00060"), #discharge
                                       startDate = "",
                                       endDate = "")  
#renaming columns to understand what they show
names(bestsites.discharge.1026)[4:5] <- c("Discharge", "Approval Code")

#huc1027
bestsites.discharge.1027 <- readNWISdv(siteNumbers = c("06892350", "06887500"), 
                                       parameterCd = c("00060"), #discharge
                                       startDate = "",
                                       endDate = "") 
#renaming columns to understand what they show
names(bestsites.discharge.1027)[4:5] <- c("Discharge", "Approval Code")

#huc1028
bestsites.discharge.1028 <- readNWISdv(siteNumbers = c("06902000",
                                                      "06905500"),
                                      parameterCd = c("00060"), #discharge
                                      startDate = "",
                                      endDate = "")
names(bestsites.discharge.1028)[4:5] <- c("Discharge", "Approval Code")

#huc1029
bestsites.discharge.1029 <- readNWISdv(siteNumbers = c("06921070",
                                                      "06926510"),
                                      parameterCd = c("00060"), #discharge
                                      startDate = "",
                                      endDate = "")
names(bestsites.discharge.1029)[4:5] <- c("Discharge", "Approval Code")

#huc1030
bestsites.discharge.1030 <- readNWISdv(siteNumbers = c("06934500", "06894000"), 
                                       parameterCd = c("00060"), #discharge
                                       startDate = "",
                                       endDate = "") 

#renaming columns to understand what they show
names(bestsites.discharge.1030)[4:5] <- c("Discharge", "Approval Code")

#Nitrogen and Phosphorous for both sites in huc1020
bestsites.NP.1020 <- readNWISqw(siteNumbers = c("06768000", 
                                                "06805500", 
                                                "06775900", 
                                                "06794000", 
                                                "06800000", 
                                                "06800500"),
                               parameterCd = c("00600", "00665"), #TN, TP
                               startDate = "",
                               endDate = "")

names(bestsites.NP.1020)[3] <- c("Date")

#huc1021
bestsites.NP.1021 <- readNWISqw(siteNumbers = c("06775900", "06794000"),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")

names(bestsites.NP.1021)[3] <- c("Date")

#huc1023
bestsites.NP.1023 <- readNWISqw(siteNumbers = c("06609500", "06610000"),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")

names(bestsites.NP.1023)[3] <- c("Date")

#huc1024 
bestsites.NP.1024 <- readNWISqw(siteNumbers = c("06818000", "06810000"),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")

names(bestsites.NP.1024)[3] <- c("Date")

#huc1025 
bestsites.NP.1025 <- readNWISqw(siteNumbers = c("06844500", "06856600"),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")

names(bestsites.NP.1025)[3] <- c("Date")

#huc1026 
bestsites.NP.1026 <- readNWISqw(siteNumbers = c("06877600", "06874000"),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")

names(bestsites.NP.1026)[3] <- c("Date")

#huc1027 
bestsites.NP.1027 <- readNWISqw(siteNumbers = c("06892350", "06887500"),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")

names(bestsites.NP.1027)[3] <- c("Date")

#huc1028
bestsites.NP.1028 <- readNWISqw(siteNumbers = c("06902000", 
                                               "06905500"),
                               parameterCd = c("00600", "00665"), #TN, TP
                               startDate = "",
                               endDate = "")

names(bestsites.NP.1028)[3] <- c("Date")

#huc1029
bestsites.NP.1029 <- readNWISqw(siteNumbers = c("06921070",
                                               "06926510"),
                               parameterCd = c("00600", "00665"), #TN, TP
                               startDate = "",
                               endDate = "")
names(bestsites.NP.1029)[3] <- c("Date")

#huc1030 
bestsites.NP.1030 <- readNWISqw(siteNumbers = c("06934500", "06894000"),
                                parameterCd = c("00600", "00665"), #TN, TP
                                startDate = "",
                                endDate = "")
names(bestsites.NP.1030)[3] <- c("Date")

#joinining datatables for water quality and discharge data
bestsites.1020 <- full_join(bestsites.discharge.1020, bestsites.NP.1020,
                                by = c("site_no", "agency_cd", "Date"))
bestsites.1021 <- full_join(bestsites.discharge.1021, bestsites.NP.1021,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1023 <- full_join(bestsites.discharge.1023, bestsites.NP.1023,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1024 <- full_join(bestsites.discharge.1024, bestsites.NP.1024,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1025 <- full_join(bestsites.discharge.1025, bestsites.NP.1025,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1026 <- full_join(bestsites.discharge.1026, bestsites.NP.1026,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1027 <- full_join(bestsites.discharge.1027, bestsites.NP.1027,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1028 <- full_join(bestsites.discharge.1028, bestsites.NP.1028,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1029 <- full_join(bestsites.discharge.1029, bestsites.NP.1029,
                            by = c("site_no", "agency_cd", "Date"))
bestsites.1030 <- full_join(bestsites.discharge.1023, bestsites.NP.1030,
                            by = c("site_no", "agency_cd", "Date"))

## ---High Frequency Data----

#reading in high frequency data for all sites

highfreq1024.1025 <-  c("06808500", "06817000")
highfreq1028.1029 <- c("06902000")
highfreq1020.1023 <-  c("06604440")
highfreq.1027.1030 <- c("06892350", "06892513", "06934500")


highfreq.1023 <- readNWISqw(siteNumbers = c("06604440"),
                               parameterCd = c("00060",
                                               "00630"), #discharge, TN
                               startDate = "",
                               endDate = "")

highfreq.1024 <- readNWISqw(siteNumbers = c("06808500", "06817000"),
                            parameterCd = c("00060",
                                            "00630"), #discharge, TN
                            startDate = "",
                            endDate = "")

highfreq.1028 <- readNWISqw(siteNumbers = c("06902000"),
                              parameterCd = c("00060",
                                              "00630"), #discharge, TN
                              startDate = "",
                              endDate = "")

highfreq.1027 <- readNWISqw(siteNumbers = c("06892350", "06892513"),
                            parameterCd = c("00060",
                                            "00630"), #discharge, TN
                            startDate = "",
                            endDate = "")

highfreq.1030 <- readNWISqw(siteNumbers = c("06934500"),
                            parameterCd = c("00060",
                                            "00630"), #discharge, TN
                            startDate = "",
                            endDate = "")
