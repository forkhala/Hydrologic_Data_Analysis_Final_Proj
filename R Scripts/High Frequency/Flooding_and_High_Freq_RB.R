#Flooding analysis


highfreqsiteinfo <- read.csv("./Data/Raw/highfreqsiteinfo.csv")
#there are 7 sites within our area of study with high frequency N data, 
#but only 4 have data during the time period of interest

str(highfreqsiteinfo)
highfreqsiteinfo$end_date <-  as.Date(highfreqsiteinfo$end_date)
highfreqsiteinfo$begin_date <-  as.Date(highfreqsiteinfo$begin_date)

#site info with dates of data
highfreqsite2019 <- highfreqsiteinfo %>%
  filter(end_date > "2019-03-31"); head(highfreqsite2019)

highfreqsites.DN <- readNWISuv(site = c("06808500", "06817000", "06892350", "06934500"), 
                               parameterCd = c("00060", "99133"), 
                               # Discharge in cfs & Nitrate in mg/l NO3-N
                               startDate = "2019-03-01",
                               endDate = "2019-08-01") %>%
                               renameNWISColumns() %>%
                               rename(Nitrate_mgl = 6)
#individual sites
Hermann <- highfreqsites.DN %>%
           filter(site_no=="06934500")
Desoto <- highfreqsites.DN %>%
          filter(site_no=="06892350")
Clarinda <- highfreqsites.DN %>%
            filter(site_no=="06817000")
Randolph <- highfreqsites.DN %>%
            filter(site_no=="06808500")


#--------plotting C-Q plots--------------#
Hermann.plot <- ggplot(Hermann,
                aes(x = Flow_Inst, y = Nitrate_mgl)) +
                geom_point() +
                scale_x_log10() +
                scale_y_log10()
print(Hermann.plot)

Desoto.plot <- ggplot(Desoto,
               aes(x = Flow_Inst, y = Nitrate_mgl)) +
               geom_point() +
               scale_x_log10() +
               scale_y_log10()
print(Desoto.plot)

Clarinda.plot <- ggplot(Clarinda,
                 aes(x = Flow_Inst, y = Nitrate_mgl)) +
                 geom_point() +
                 scale_x_log10() +
                 scale_y_log10()
print(Clarinda.plot)

Randolph.plot <- ggplot(Randolph,
                 aes(x = Flow_Inst, y = Nitrate_mgl)) +
                 geom_point() +
                 scale_x_log10() +
                 scale_y_log10()
print(Randolph.plot)


