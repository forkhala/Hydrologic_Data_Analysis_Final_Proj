options(scipen = 100)

#### Drought Plots for site Missouri River at Hermann, MO, # 06934500 ####
#reading in site info for 06817000
stationInfo.Missouri <- readNWISsite(siteNumber = "06934500")

#filtering for just one site and completing date sequence
site.dis.Missouri <- site.dis %>%
  filter(site_no == "06934500")

#checking to see if there are missing dates in the dataframe
table(diff(site.dis.Missouri$Date))

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(site.dis.Missouri$Date))) != (nrow(site.dis.Missouri)+1)){
  fullDates.dis.Missouri <- seq(from = min(site.dis.Missouri$Date),
                               to = max(site.dis.Missouri$Date), by="1 day")
  fullDates.dis.Missouri <- data.frame(Date = fullDates.dis.Missouri, 
                                      agency_cd = site.dis.Missouri$agency_cd[1],
                                      site_no = site.dis.Missouri$site_no[1],
                                      stringsAsFactors = FALSE)
  dailyQ.Missouri <- full_join(site.dis.Missouri, fullDates.dis.Missouri,
                              by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

#calculating 30 day average
dailyQ.30.avg.Missouri <- dailyQ.Missouri %>%
  na.omit() %>%
  mutate(rM = rollmean(Flow, 30, na.pad = TRUE, align = "center"),
         day.of.year = as.numeric(strftime(Date, 
                                           format = "%j")))

#summarizing historical data
summaryQ.Missouri <- dailyQ.7.avg.Missouri %>%
  group_by(day.of.year) %>%
  summarize(p75 = quantile(rM, probs = .75, na.rm = TRUE),
            p25 = quantile(rM, probs = .25, na.rm = TRUE),
            p10 = quantile(rM, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rM, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rM, probs = 0, na.rm = TRUE)) 

#setting current year to 2018
current.year <- as.numeric(2018, format = "%Y") 

#summarizing data for 2011 - 2013
summary.0.Missouri <- summaryQ.Missouri %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-2,"-01-01")),
         day.of.year = day.of.year - 365)

#summarizing data for 2012 - 2015
summary.1.Missouri <- summaryQ.Missouri %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-1,"-01-01")))

#summarizing data for 2018 - 2020
summary.2.Missouri <- summaryQ.Missouri %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year,"-01-01")),
         day.of.year = day.of.year + 365)

#putting data for 2017, 2018, and 2019 in one table
summaryQ.Missouri <- bind_rows(summary.0.Missouri, summary.1.Missouri, summary.2.Missouri) 

smooth.span <- 0.3

#predicting percentiles for 2017 -2019 low flow data
summaryQ.Missouri$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ.Missouri, span = smooth.span))
summaryQ.Missouri$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ.Missouri, span = smooth.span))
summaryQ.Missouri$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ.Missouri, span = smooth.span))
summaryQ.Missouri$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ.Missouri, span = smooth.span))
summaryQ.Missouri$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ.Missouri, span = smooth.span))

#filtering to only have certain columns in the data table for 2018 - 2020 discharge
summaryQ.Missouri <- select(summaryQ.Missouri, Date, day.of.year,
                           sm.75, sm.25, sm.10, sm.05, sm.00) %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01")))

#filtering to only have latest years in one dataframe 
latest.years.Missouri <- dailyQ.7.avg.Missouri %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01"))) %>%
  mutate(day.of.year = 1:nrow(.))

#plotting low flow data and intervals 

drought.plot.Missouri <- ggplot(data = summaryQ.Missouri, aes(x = day.of.year)) +
  geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) +
  geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) +
  geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) +
  geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) +
  scale_y_log10(limits = c(1000,400000)) +
  geom_line(data = latest.years.Missouri, aes(x=day.of.year, 
                                             y=rM, color = "7-Day Mean"),size=2) + #plotting 7-day mean for 2018-2019
  geom_vline(xintercept = 365) 

print(drought.plot.Missouri)

#info for site 06808500
mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

title.text.Missouri <- paste0(stationInfo.Missouri$station_nm,"\n",
                             "Record Start = ", min(dailyQ.7.avg.Missouri$Date),
                             "  Number of years = ",
                             as.integer(as.numeric(difftime(time1 = max(dailyQ.7.avg.Missouri$Date), 
                                                            time2 = min(dailyQ.7.avg.Missouri$Date),
                                                            units = "weeks"))/52.25),
                             "\nDate of plot = ",Sys.Date(),
                             "  Drainage Area = ",stationInfo.Missouri$drain_area_va, "mi^2")

#plotting a better plot for 06808500
styled.plot.Missouri <- drought.plot.Missouri +
  scale_x_continuous(breaks = c(mid.month.days, 365+mid.month.days),
                     labels = rep(month.letters, 2),
                     expand = c(0, 0),
                     limits = c(0,730)) +
  annotation_logticks(sides = "l") +
  expand_limits(x = 0) +
  annotate(geom = "text", 
           x = c(182,547), 
           y = 1000, 
           label = c(current.year-1, current.year), size = 4) +
  theme_bw() + 
  theme(axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle(title.text.Missouri) +
  labs(list(title = title.text.Missouri),
       y = "30-day moving average", x = "Month") +
  scale_fill_manual(name = "", breaks = label.text,
                    values = c("red","orange","yellow","darkgreen")) +
  scale_color_manual(name = "", values = "black") +
  theme(legend.position="bottom")

print(styled.plot.Missouri)
