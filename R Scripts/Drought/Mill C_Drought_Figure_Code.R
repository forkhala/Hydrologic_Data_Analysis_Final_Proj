options(scipen = 100)

#### Drought PLots for site Mill C at Johnson Drive, Shawnee, KS, # 06892513 ####

#reading in site info for 06892513 
stationInfo.Mill <- readNWISsite(siteNumber = "06892513")

#filtering for just one site and completing date sequence
site.dis.Mill <- site.dis %>%
  filter(site_no == "06892513")

#checking to see if there are missing dates in the dataframe
table(diff(site.dis.Mill$Date))

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(site.dis.Mill$Date))) != (nrow(site.dis.Mill)+1)){
  fullDates.dis.Mill <- seq(from = min(site.dis.Mill$Date),
                              to = max(site.dis.Mill$Date), by="1 day")
  fullDates.dis.Mill <- data.frame(Date = fullDates.dis.Mill, 
                                     agency_cd = site.dis.Mill$agency_cd[1],
                                     site_no = site.dis.Mill$site_no[1],
                                     stringsAsFactors = FALSE)
  dailyQ.Mill <- full_join(site.dis.Mill, fullDates.dis.Mill,
                             by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}


#calculating 30 day average
dailyQ.30.avg.Mill <- dailyQ.Mill %>%
  mutate(rM = rollmean(Flow, 30, na.pad = TRUE, align = "center"),
         day.of.year = as.numeric(strftime(Date, 
                                           format = "%j")))

#summarizing historical data
summaryQ.Mill <- dailyQ.30.avg.Mill %>%
  group_by(day.of.year) %>%
  summarize(p75 = quantile(rM, probs = .75, na.rm = TRUE),
            p25 = quantile(rM, probs = .25, na.rm = TRUE),
            p10 = quantile(rM, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rM, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rM, probs = 0, na.rm = TRUE)) 

#setting current year to 2018
current.year <- as.numeric(2018, format = "%Y") 

#summarizing data for 2017 - 2018
summary.0.Mill <- summaryQ.Mill %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-2,"-01-01")),
         day.of.year = day.of.year - 365)

#summarizing data for 2012 - 2015
summary.1.Mill <- summaryQ.Mill %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-1,"-01-01")))

#summarizing data for 2018 - 2020
summary.2.Mill <- summaryQ.Mill %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year,"-01-01")),
         day.of.year = day.of.year + 365)

#putting data for 2017, 2018, and 2019 in one table
summaryQ.Mill <- bind_rows(summary.0.Mill, summary.1.Mill, summary.2.Mill) 

smooth.span <- 0.3

#predicting percentiles for 2017 -2019 low flow data
summaryQ.Mill$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ.Mill, span = smooth.span))
summaryQ.Mill$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ.Mill, span = smooth.span))
summaryQ.Mill$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ.Mill, span = smooth.span))
summaryQ.Mill$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ.Mill, span = smooth.span))
summaryQ.Mill$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ.Mill, span = smooth.span))

#filtering to only have certain columns in the data table for 2018 - 2020 discharge
summaryQ.Mill <- select(summaryQ.Mill, Date, day.of.year,
                          sm.75, sm.25, sm.10, sm.05, sm.00) %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01")))

#filtering to only have latest years in one dataframe (2018 - 2019 data)
latest.years.Mill <- dailyQ.30.avg.Mill %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01"))) %>%
  mutate(day.of.year = 1:nrow(.))

#info for site 06892513
mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

title.text.Mill <- paste0(stationInfo.Mill$station_nm,"\n",
                            "Record Start = ", min(dailyQ.30.avg.Mill$Date),
                            "  Number of years = ",
                            as.integer(as.numeric(difftime(time1 = max(dailyQ.30.avg.Mill$Date), 
                                                           time2 = min(dailyQ.30.avg.Mill$Date),
                                                           units = "weeks"))/52.25),
                            "\nDate of plot = ",Sys.Date(),
                            "  Drainage Area = ",stationInfo.Mill$drain_area_va, "mi^2")


#plotting low flow data and intervals 

drought.plot.Mill <- ggplot(data = summaryQ.Mill, aes(x = day.of.year)) +
  geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) +
  geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) +
  geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) +
  geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) +
  scale_y_log10(limits = c(1, 1000)) +
  geom_line(data = latest.years.Mill, aes(x=day.of.year, 
                                            y=rM, color = "30-Day Mean"),size=2) + #plotting 7-day mean for 2018-2019
  geom_vline(xintercept = 365) 

print(drought.plot.Mill)


#plotting a better plot for Mill River
styled.plot.Mill <- drought.plot.Mill +
  scale_x_continuous(breaks = c(mid.month.days, 365+mid.month.days),
                     labels = rep(month.letters, 2),
                     expand = c(0, 0),
                     limits = c(0,731)) +
  annotation_logticks(sides = "l") +
  expand_limits(x = 0) +
  annotate(geom = "text", 
           x = c(182,547), 
           y = 1, 
           label = c(current.year-1, current.year), size = 4) +
  theme_bw() + 
  theme(axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle(title.text.Mill) +
  labs(list(title = title.text.Mill),
       y = "30-day moving average", x = "Month") +
  scale_fill_manual(name = "", breaks = label.text,
                    values = c("red","orange","yellow","darkgreen")) +
  scale_color_manual(name = "", values = "black") +
  theme(legend.position="bottom")

print(styled.plot.Mill)


