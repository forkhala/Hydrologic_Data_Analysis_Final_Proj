options(scipen = 100)

#### Drought Plots for site West Nishnabotna River in Randolph, IA | 06808500 ####

#reading in site info for 06808500
stationInfo.Nish <- readNWISsite(siteNumber = "06808500")

#filtering for just one site and completing date sequence
site.dis.Nish <- site.dis %>%
  filter(site_no == "06808500")

#checking to see if there are missing dates in the dataframe
table(diff(site.dis.Nish$Date))

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(site.dis.Nish$Date))) != (nrow(site.dis.Nish)+1)){
  fullDates.dis.Nish <- seq(from = min(site.dis.Nish$Date),
                                to = max(site.dis.Nish$Date), by="1 day")
  fullDates.dis.Nish <- data.frame(Date = fullDates.dis.Nish, 
                                       agency_cd = site.dis.Nish$agency_cd[1],
                                       site_no = site.dis.Nish$site_no[1],
                                       stringsAsFactors = FALSE)
  dailyQ.Nish <- full_join(site.dis.Nish, fullDates.dis.Nish,
                               by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

#calculating 30 day average
dailyQ.30.avg.Nish <- dailyQ.Nish %>%
  na.omit() %>%
  mutate(rM = rollmean(Flow, 30, na.pad = TRUE, align = "center"),
         day.of.year = as.numeric(strftime(Date, 
                                           format = "%j")))

#summarizing historical data
summaryQ.Nish <- dailyQ.7.avg.Nish %>%
  group_by(day.of.year) %>%
  summarize(p75 = quantile(rM, probs = .75, na.rm = TRUE),
            p25 = quantile(rM, probs = .25, na.rm = TRUE),
            p10 = quantile(rM, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rM, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rM, probs = 0, na.rm = TRUE)) 

#setting current year to 2018
current.year <- as.numeric(2018, format = "%Y") 

#summarizing data for 2011 - 2013
summary.0.Nish <- summaryQ.Nish %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-2,"-01-01")),
         day.of.year = day.of.year - 365)

#summarizing data for 2012 - 2015
summary.1.Nish <- summaryQ.Nish %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-1,"-01-01")))

#summarizing data for 2018 - 2020
summary.2.Nish <- summaryQ.Nish %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year,"-01-01")),
         day.of.year = day.of.year + 365)

#putting data for 2017, 2018, and 2019 in one table
summaryQ.Nish <- bind_rows(summary.0.Nish, summary.1.Nish, summary.2.Nish) 

smooth.span <- 0.3

#predicting percentiles for 2017 -2019 low flow data
summaryQ.Nish$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ.Nish, span = smooth.span))
summaryQ.Nish$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ.Nish, span = smooth.span))
summaryQ.Nish$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ.Nish, span = smooth.span))
summaryQ.Nish$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ.Nish, span = smooth.span))
summaryQ.Nish$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ.Nish, span = smooth.span))

#filtering to only have certain columns in the data table for 2018 - 2020 discharge
summaryQ.Nish <- select(summaryQ.Nish, Date, day.of.year,
                            sm.75, sm.25, sm.10, sm.05, sm.00) %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01")))

#filtering to only have latest years in one dataframe 
latest.years.Nish <- dailyQ.7.avg.Nish %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01"))) %>%
  mutate(day.of.year = 1:nrow(.))

#plotting low flow data and intervals 

drought.plot.Nish <- ggplot(data = summaryQ.Nish, aes(x = day.of.year)) +
  geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) +
  geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) +
  geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) +
  geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) +
  scale_y_log10(limits = c(10,10000)) +
  geom_line(data = latest.years.Nish, aes(x=day.of.year, 
                                              y=rM, color = "7-Day Mean"),size=2) + #plotting 7-day mean for 2018-2019
  geom_vline(xintercept = 365) 

print(drought.plot.Nish)

#info for site 06808500
mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

title.text.Nish <- paste0(stationInfo.Nish$station_nm,"\n",
                              "Record Start = ", min(dailyQ.7.avg.Nish$Date),
                              "  Number of years = ",
                              as.integer(as.numeric(difftime(time1 = max(dailyQ.7.avg.Nish$Date), 
                                                             time2 = min(dailyQ.7.avg.Nish$Date),
                                                             units = "weeks"))/52.25),
                              "\nDate of plot = ",Sys.Date(),
                              "  Drainage Area = ",stationInfo.Nish$drain_area_va, "mi^2")

#plotting a better plot for 06808500
styled.plot.Nish <- drought.plot.Nish +
  scale_x_continuous(breaks = c(mid.month.days, 365+mid.month.days),
                     labels = rep(month.letters, 2),
                     expand = c(0, 0),
                     limits = c(0,730)) +
  annotation_logticks(sides = "l") +
  expand_limits(x = 0) +
  annotate(geom = "text", 
           x = c(182,547), 
           y = 10, 
           label = c(current.year-1, current.year), size = 4) +
  theme_bw() + 
  theme(axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle(title.text.Nish) +
  labs(list(title = title.text.Nish),
       y = "30-day moving average", x = "Month") +
  scale_fill_manual(name = "", breaks = label.text,
                    values = c("red","orange","yellow","darkgreen")) +
  scale_color_manual(name = "", values = "black") +
  theme(legend.position="bottom")

print(styled.plot.Nish)
