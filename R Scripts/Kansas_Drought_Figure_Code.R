#### Drought Plots for site Kansas River in Desoto, KS, # 06892350 ####

#reading in site info for 06892350 
stationInfo.Kansas <- readNWISsite(siteNumber = "06892350")

#filtering for just one site and completing date sequence
site.dis.Kansas <- site.dis %>%
  filter(site_no == "06892350")

#could add this code to the filter above, but don't think it's working
#%>%
mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day"))

#check for missing dates in dataframe
d <- site.1.dis$Date
date_range <- seq(min(d), max(d), by = 1) 
date_range[!date_range %in% d] 

#checking to see if there are missing dates in the dataframe
table(diff(site.dis.Kansas$Date))

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(site.dis.Kansas$Date))) != (nrow(site.dis.Kansas)+1)){
  fullDates.dis.Kansas <- seq(from = min(site.dis.Kansas$Date),
                               to = max(site.dis.Kansas$Date), by="1 day")
  fullDates.dis.Kansas <- data.frame(Date = fullDates.dis.Kansas, 
                                      agency_cd = site.dis.Kansas$agency_cd[1],
                                      site_no = site.dis.Kansas$site_no[1],
                                      stringsAsFactors = FALSE)
  dailyQ.Kansas <- full_join(site.dis.Kansas, fullDates.dis.Kansas,
                              by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

#calculating 7 day average
dailyQ.7.avg.Kansas <- dailyQ.Kansas %>%
  mutate(rM = rollmean(Flow, 7, na.pad = TRUE, align = "center"),
         day.of.year = as.numeric(strftime(Date, 
                                           format = "%j")))

#summarizing historical data
summaryQ.Kansas <- dailyQ.7.avg.Kansas %>%
  group_by(day.of.year) %>%
  summarize(p75 = quantile(rM, probs = .75, na.rm = TRUE),
            p25 = quantile(rM, probs = .25, na.rm = TRUE),
            p10 = quantile(rM, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rM, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rM, probs = 0, na.rm = TRUE)) 

#looking at current year (2019) data
current.year <- as.numeric(2013, format = "%Y")

#summarizing data for 2011 - 2013
summary.0.Kansas <- summaryQ.Kansas %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-7,"-01-01")),
         day.of.year = day.of.year - 365)

#summarizing data for 2012 - 2015
summary.1.Kansas <- summaryQ.Kansas %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-6,"-01-01")))

#summarizing data for 2018 - 2020
summary.2.Kansas <- summaryQ.Kansas %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year,"-01-01")),
         day.of.year = day.of.year + 365)

#putting data for 2017, 2018, and 2019 in one table
summaryQ.Kansas <- bind_rows(summary.0.Kansas, summary.1.Kansas, summary.2.Kansas) 

smooth.span <- 0.3

#predicting percentiles for 2017 -2019 low flow data
summaryQ.Kansas$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ.Kansas, span = smooth.span))
summaryQ.Kansas$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ.Kansas, span = smooth.span))
summaryQ.Kansas$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ.Kansas, span = smooth.span))
summaryQ.Kansas$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ.Kansas, span = smooth.span))
summaryQ.Kansas$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ.Kansas, span = smooth.span))

#filtering to only have certain columns in the data table for 2018 - 2020 discharge
summaryQ.Kansas <- select(summaryQ.Kansas, Date, day.of.year,
                           sm.75, sm.25, sm.10, sm.05, sm.00) %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01")))

#filtering to only have latest years in one dataframe (2018 - 2019 data)
latest.years.Kansas <- dailyQ.7.avg.Kansas %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01"))) %>%
  mutate(day.of.year = 1:nrow(.))

#plotting low flow data and intervals 

drought.plot.Kansas <- ggplot(data = summaryQ.Kansas, aes(x = day.of.year)) +
  geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) +
  geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) +
  geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) +
  geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) +
  scale_y_log10(limits = c(1,60000)) +
  geom_line(data = latest.years.Kansas, aes(x=day.of.year, 
                                             y=rM, color = "7-Day Mean"),size=2) + #plotting 7-day mean for 2018-2019
  geom_vline(xintercept = 365) 

print(drought.plot.Kansas)

#info for site 06808500
mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

title.text.Kansas <- paste0(stationInfo.Kansas$station_nm,"\n",
                             "Record Start = ", min(dailyQ.7.avg.Kansas$Date),
                             "  Number of years = ",
                             as.integer(as.numeric(difftime(time1 = max(dailyQ.7.avg.Kansas$Date), 
                                                            time2 = min(dailyQ.7.avg.Kansas$Date),
                                                            units = "weeks"))/52.25),
                             "\nDate of plot = ",Sys.Date(),
                             "  Drainage Area = ",stationInfo.Kansas$drain_area_va, "mi^2")

#plotting a better plot for Kansas River
styled.plot.Kansas <- drought.plot.Kansas +
  scale_x_continuous(breaks = c(mid.month.days, 365+mid.month.days),
                     labels = rep(month.letters, 2),
                     expand = c(0, 0),
                     limits = c(0,730)) +
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
  labs(list(title = title.text.Kansas),
       y = "7-day moving average", x = "Month") +
  scale_fill_manual(name = "", breaks = label.text,
                    values = c("red","orange","yellow","darkgreen")) +
  scale_color_manual(name = "", values = "black") +
  theme(legend.position="bottom")

print(styled.plot.Kansas)

