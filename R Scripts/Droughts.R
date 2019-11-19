getwd()
library(tidyverse)
library(dataRetrieval)
library(dplyr)
library(xts)
library(dygraphs)
library(lubridate)
library(kableExtra)
library(gridExtra)
library(tidyr)
#install.packages("data.table")
library(data.table)
#install.packages("dtplyr")
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(trend)
library(TTR)

#setting theme
theme_set(theme_classic())

#### Reading in data and data wrangling ####

#compiling site numbers together for drought analysis
sites <- c("06808500", "06817000", "06892350", "06934500", "06892513", "06902000")

#reading in data for sites that were analyzed for floods
site.dis <- readNWISdv(siteNumbers = c(sites),
                       parameterCd = "00060", #discharge
                       startDate = "",
                       endDate = "") %>%
  renameNWISColumns()

####Exploratory data analysis (droughts)####

#reading in site info for 06808500
stationInfo <- readNWISsite(siteNumber = "06808500")

#filtering for just one site and completing date sequence
site.1.dis <- site.dis %>%
  filter(site_no == "06808500") 

#could add this code to the filter above, but don't think it's working
#%>%
mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day"))

#check for missing dates in dataframe
d <- site.dis$Date
date_range <- seq(min(d), max(d), by = 1) 
date_range[!date_range %in% d] 

#checking to see if there are missing dates in the dataframe
table(diff(site.1.dis$Date))

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(site.1.dis$Date))) != (nrow(site.1.dis)+1)){
  fullDates.dis <- seq(from = min(site.1.dis$Date),
                       to = max(site.1.dis$Date), by="1 day")
  fullDates.dis <- data.frame(Date = fullDates.dis, 
                              agency_cd = site.1.dis$agency_cd[1],
                              site_no = site.1.dis$site_no[1],
                              stringsAsFactors = FALSE)
  dailyQ <- full_join(site.1.dis, fullDates.dis,
                      by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}


if(as.numeric(diff(range(bestsites.discharge$Date))) != (nrow(bestsites.discharge)+1)){
  fullDates <- seq(from=min(bestsites.discharge$Date),
                   to = max(bestsites.discharge$Date), by="1 day")
  fullDates <- data.frame(Date = fullDates, 
                          agency_cd = bestsites.discharge$agency_cd[1],
                          site_no = bestsites.discharge$site_no[1],
                          stringsAsFactors = FALSE)
  dailyQ <- full_join(bestsites.discharge, fullDates,
                      by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

#calculate a moving average to determine 7 day average 
ma <- function(x, n=7){stats::filter(x, rep(1/n, n), sides=1)}


dailyQ.7.avg <- dailyQ %>%
  mutate(rollMean = as.numeric(ma(Flow)),
         day.of.year = as.numeric(strftime(Date, 
                                           format = "%j")))

#summarizing historical data
summaryQ <- dailyQ.7.avg %>%
  group_by(day.of.year) %>%
  summarize(p75 = quantile(rollMean, probs = .75, na.rm = TRUE),
            p25 = quantile(rollMean, probs = .25, na.rm = TRUE),
            p10 = quantile(rollMean, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rollMean, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rollMean, probs = 0, na.rm = TRUE)) 

#looking at current year (2019) data
current.year <- as.numeric(strftime(Sys.Date(), format = "%Y"))

#summarizing data for 2017
summary.0 <- summaryQ %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-2,"-01-01")),
         day.of.year = day.of.year - 365)

#summarizing data for 2018
summary.1 <- summaryQ %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-1,"-01-01")))

#summarizing data for 2019
summary.2 <- summaryQ %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year,"-01-01")),
         day.of.year = day.of.year + 365)

#putting data for 2017, 2018, and 2019 in one table
summaryQ <- bind_rows(summary.0, summary.1, summary.2) 

smooth.span <- 0.3

#predicting percentiles for 2017 -2019 low flow data
summaryQ$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ, span = smooth.span))
summaryQ$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ, span = smooth.span))

#filtering to only have certain columns in the data table for 2018 - 2020 discharge
summaryQ <- select(summaryQ, Date, day.of.year,
                   sm.75, sm.25, sm.10, sm.05, sm.00) %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01")))

#filtering to only have latest years in one dataframe (2018 - 2019 data)
latest.years <- dailyQ.7.avg %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01"))) %>%
  mutate(day.of.year = 1:nrow(.))

#plotting low flow data and intervals 

drought.plot <- ggplot(data = summaryQ, aes(x = day.of.year)) +
  geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) +
  geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) +
  geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) +
  geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) +
  scale_y_log10(limits = c(1,1000)) +
  geom_line(data = latest.years, aes(x=day.of.year, 
                                     y=rollMean, color = "7-Day Mean"),size=2) +
  geom_vline(xintercept = 365) 

print(drought.plot)

#info for plotting 
mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

title.text <- paste0(stationInfo$station_nm,"\n",
                     "Record Start = ", min(dailyQ$Date),
                     "  Number of years = ",
                     as.integer(as.numeric(difftime(time1 = max(dailyQ$Date), 
                                                    time2 = min(dailyQ$Date),
                                                    units = "weeks"))/52.25),
                     "\nDate of plot = ",Sys.Date(),
                     "  Drainage Area = ",stationInfo$drain_area_va, "mi^2")

#plotting a better plot for 06808500
styled.plot <- drought.plot +
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
  labs(list(title = title.text,
            y = "7-day moving average", x = "Month")) +
  scale_fill_manual(name = "", breaks = label.text,
                    values = c("red","orange","yellow","darkgreen")) +
  scale_color_manual(name = "", values = "black") +
  theme(legend.position="bottom")

print(styled.plot)

#### Drought Plots for site West Nishnabotna River in Randolph, IA | 06808500 ####

#filtering for just one site 
Nish.dis <- site.dis %>%
  filter(site_no == "06808500")

#Check for missing days, if so, add NA rows:
if(as.numeric(diff(range(Nish.dis$Date))) != (nrow(Nish.dis)+1)){
  fullDates.dis <- seq(from=min(Nish.dis$Date),
                       to = max(Nish.dis$Date), by="1 day")
  fullDates.dis <- data.frame(Date = fullDates.dis, 
                              agency_cd = Nish.dis$agency_cd[1],
                              site_no = Nish.dis$site_no[1],
                              stringsAsFactors = FALSE)
  dailyQ <- full_join(Nish.dis, fullDates.dis,
                      by=c("Date","agency_cd","site_no")) %>%
    arrange(Date)
}

#calculate a moving average to determine 7 day average 
ma <- function(x, n=7){stats::filter(x, rep(1/n, n), sides=1)}


dailyQ.7.avg.Nish <- dailyQ %>%
  mutate(rollMean = as.numeric(ma(Flow)),
         day.of.year = as.numeric(strftime(Date, 
                                           format = "%j")))

#summarizing historical data
summaryQ.Nish <- dailyQ.7.avg.Nish %>%
  group_by(day.of.year) %>%
  summarize(p75 = quantile(rollMean, probs = .75, na.rm = TRUE),
            p25 = quantile(rollMean, probs = .25, na.rm = TRUE),
            p10 = quantile(rollMean, probs = 0.1, na.rm = TRUE),
            p05 = quantile(rollMean, probs = 0.05, na.rm = TRUE),
            p00 = quantile(rollMean, probs = 0, na.rm = TRUE)) 

#looking at current year (2019) data
current.year <- as.numeric(strftime(Sys.Date(), format = "%Y"))

#summarizing data for 2017
summary.0 <- summaryQ.Nish %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-2,"-01-01")),
         day.of.year = day.of.year - 365)

#summarizing data for 2018
summary.1 <- summaryQ.Nish %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year-1,"-01-01")))

#summarizing data for 2019
summary.2 <- summaryQ.Nish %>%
  mutate(Date = as.Date(day.of.year - 1, 
                        origin = paste0(current.year,"-01-01")),
         day.of.year = day.of.year + 365)

#putting data for 2017, 2018, and 2019 in one table
summaryQ.Nish <- bind_rows(summary.0, summary.1, summary.2) 

smooth.span <- 0.3

#predicting percentiles for 2017 -2019 low flow data
summaryQ.Nish$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ, span = smooth.span))
summaryQ.Nish$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ, span = smooth.span))
summaryQ.Nish$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ, span = smooth.span))
summaryQ.Nish$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ, span = smooth.span))
summaryQ.Nish$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ, span = smooth.span))

#filtering to only have certain columns in the data table for 2018 - 2020 discharge
summaryQ.Nish <- select(summaryQ.Nish, Date, day.of.year,
                        sm.75, sm.25, sm.10, sm.05, sm.00) %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01")))

#filtering to only have latest years in one dataframe (2018 - 2019 data)
latest.years <- dailyQ.7.avg.Nish %>%
  filter(Date >= as.Date(paste0(current.year-1,"-01-01"))) %>%
  mutate(day.of.year = 1:nrow(.))

#plotting low flow data and intervals 

drought.plot.Nish <- ggplot(data = summaryQ.Nish, aes(x = day.of.year)) +
  geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) +
  geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) +
  geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) +
  geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) +
  scale_y_log10(limits = c(1,1000)) +
  geom_line(data = latest.years, aes(x=day.of.year, 
                                     y=rollMean, color = "7-Day Mean"),size=2) +
  geom_vline(xintercept = 365) 

print(drought.plot)

#info for plotting 
mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

title.text <- paste0(stationInfo$station_nm,"\n",
                     "Record Start = ", min(dailyQ$Date),
                     "  Number of years = ",
                     as.integer(as.numeric(difftime(time1 = max(dailyQ$Date), 
                                                    time2 = min(dailyQ$Date),
                                                    units = "weeks"))/52.25),
                     "\nDate of plot = ",Sys.Date(),
                     "  Drainage Area = ",stationInfo$drain_area_va, "mi^2")

#plotting a better plot for 06921070
styled.plot <- drought.plot +
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
  labs(list(title = title.text,
            y = "7-day moving average", x = "Month")) +
  scale_fill_manual(name = "", breaks = label.text,
                    values = c("red","orange","yellow","darkgreen")) +
  scale_color_manual(name = "", values = "black") +
  theme(legend.position="bottom")

print(styled.plot)
#### Drought Plots for site Nodaway River at Clarinda, IA, # 06817000 ####
#### Drought Plots for site Kansas River in Desoto, KS, # 06892350 ####
#### Drought Plots for site Missouri River at Hermann, MO, # 6934500  ####
#### Drought PLots for site Mill C at Johnson Drive, Shawnee, KS, # 06892513 #### 
#### Drought Plots for site Grand River, Sumner MO  | 06902000 ####

#### 7Q10 Analysis for site all chosen sites #### 

#filtering dataframe to get 7 day average
avg.dis <- bestsites.discharge %>%
  select(site_no, Date, Flow) %>%
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date)) %>%
  arrange(Date) %>%
  mutate(rM = rollmean(Flow, 7, na.pad = TRUE, align = "right")) 


na.omit %>%
  group_by(Year) %>%
  summarise(min(rM))

#creating a dataframe with just flow data and   
Q.7.avg <- avg.dis %>%
  select(site_no, Date, Flow, Year, Month, Day, rM) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(min(rM))

#renaming second column
names(Q.7.avg)[2] <- c("min.sv.avg")

#mutating columns to include a rank column
Q.7.avg.prob <- Q.7.avg %>%
  arrange(Q.7.avg$min.sv.avg) %>%
  mutate(rank = 1:length(Q.7.avg$min.sv.avg),
         count = n(),
         Tr = ((count + 1)/rank),
         Prob = (1/Tr))

#plot of minimum 7 day average flow over time
sv.avg.plot <- ggplot(Q.7.avg.prob, aes(x = Year, y = min.sv.avg)) +
  geom_line() +
  labs(x = "Year", y = "Minimum of 7 Day Average Flow (cfs)")
print(sv.avg.plot)

#plot of recurrence interval
recurr.plot <- ggplot(Q.7.avg.prob, aes(x = Tr, y = min.sv.avg)) +
  geom_point() +
  labs(x = "Recurrence Interval", y = "Minimum of 7 Day Average Flow (cfs)")
print(recurr.plot)


#####Sites to do 7Q10 Analysis on (same as Flood Analysis)####
Site Name   |   Site Number   |   Time Period   |   Direction   |  Slope
----------- | --------------- | --------------- | ------------- | ---------
  West Nishnabotna River in Randolph, IA | 06808500  | Oct 7 - 13, 2018 | counter clockwise | negative (Figure xx)
Nodaway River at Clarinda, IA  | 06817000 | Oct 8 - 12, 2018 | clockwise | negative
Kansas River in Desoto, KS  | 06892350 |   Nov 30 - Dec 5, 2018 | counter clockwise | positive
Missouri River at Hermann, MO  | 06934500  |  Oct 7 - 20, 2018  | counter clockwise | negative
Mill C at Johnson Drive, Shawnee, KS  | 06892513 | Nov 27 - Dec 4, 2018 | clockwise | negative
Grand River, Sumner MO  | 06902000 |  Sep 6 - 10, 2018 | clockwise | positive

#### 7Q10 Analysis for site West Nishnabotna River in Randolph, IA, # 06808500 #### 

#filtering dataframe to get 7 day average
avg.dis.Nish <- site.dis %>%
  filter(site_no == '06808500') %>%
  select(site_no, Date, Flow) %>%
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date)) %>%
  arrange(Date) %>%
  mutate(rM = rollmean(Flow, 7, na.pad = TRUE, align = "right")) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(MinDischarge = min(rM)) %>%
  mutate(Rank = rank(MinDischarge),
         RecurrenceInterval = (length(Year) + 1)/Rank,
         Probability = 1/RecurrenceInterval)

#creating a dataframe with just flow data   
Q.7.avg.Nish <- avg.dis.Nish %>%
  select(site_no, Date, Flow, Year, Month, Day, rM) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(min(rM))

#renaming second column
names(Q.7.avg.Nish)[2] <- c("min.sv.avg")

#mutating columns to include a rank column
Q.7.avg.prob.Nish <- Q.7.avg.Nish %>%
  arrange(Q.7.avg.Nish$min.sv.avg) %>%
  mutate(rank = 1:length(Q.7.avg.Nish$min.sv.avg),
         count = n(),
         Tr = ((count + 1)/rank),
         Prob = (1/Tr))


group_by(Year) %>%
  summarise(PeakDischarge = max(Discharge)) %>% #create new column and compute peak discharge
  mutate(Rank = rank(-PeakDischarge), #rank peak discharge measurements from 1-30; we want 1 to be highest peak discharge in any year the and 30 to be lowest peak discharge in any year
         RecurrenceInterval = (length(Year) + 1)/Rank, #take equation from above and compute recurrence interval; we would expect to wait 31 years to see 6210 cfs peak discharge; for other years, we may see more peak discharge amounts; in a given year, we have a 96% probability that we will see 818 peak discharge or higher in a given year
         Probability = 1/RecurrenceInterval)

#plot of minimum 7 day average flow over time
sv.avg.plot.Nish <- ggplot(Q.7.avg.prob.Nish, aes(x = Year, y = min.sv.avg)) +
  geom_line() +
  labs(x = "Year", y = "Minimum of 7 Day Average Flow (cfs)")
print(sv.avg.plot.Nish)

#plot of recurrence interval
recurr.plot.Nish <- ggplot(Q.7.avg.prob.Nish, aes(x = Tr, y = min.sv.avg)) +
  geom_point() +
  labs(x = "Recurrence Interval", y = "Minimum of 7 Day Average Flow (cfs)")
print(recurr.plot.Nish)



#### 7Q10 Analysis for site Nodaway River at Clarinda, IA, # 06817000 ####
#filtering dataframe to get 7 day average for 06817000
avg.dis.Nodaway <- site.dis %>%
  filter(site_no == "06817000") %>%
  select(site_no, Date, Flow) %>%
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date),
         rM = rollmean(Flow, 7, na.pad = TRUE, align = "right")) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(min(rM))

#renaming second column
names(avg.dis.Nodaway)[2] <- c("min.sv.avg")

#mutating columns to include a rank column
Q.7.avg.prob.Nodaway <- avg.dis.Nodaway %>%
  arrange(avg.dis.Nodaway$min.sv.avg) %>%
  mutate(rank = 1:length(avg.dis.Nodaway$min.sv.avg),
         count = n(),
         Tr = ((count + 1)/rank),
         Prob = (1/Tr))

#plot of minimum 7 day average flow over time
sv.avg.plot <- ggplot(Q.7.avg.prob, aes(x = Year, y = min.sv.avg)) +
  geom_line() +
  labs(x = "Year", y = "Minimum of 7 Day Average Flow (cfs)")
print(sv.avg.plot)

#plot of recurrence interval
recurr.plot <- ggplot(Q.7.avg.prob, aes(x = Tr, y = min.sv.avg)) +
  geom_point() +
  labs(x = "Recurrence Interval", y = "Minimum of 7 Day Average Flow (cfs)")
print(recurr.plot)

#### 7Q10 Analysis for site Kansas River in Desoto, KS, # 06892350 ####

#filtering dataframe to get 7 day average for 06892350
avg.dis.Kansas <- site.dis %>%
  filter(site_no == "06892350") %>%
  select(site_no, Date, Flow) %>%
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date),
         rM = rollmean(Flow, 7, na.pad = TRUE, align = "right")) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(min(rM))

#renaming second column
names(avg.dis.Kansas)[2] <- c("min.sv.avg")

#mutating columns to include a rank column
Q.7.avg.prob.Kansas <- avg.dis.Kansas %>%
  arrange(avg.dis.Kansas$min.sv.avg) %>%
  mutate(rank = 1:length(avg.dis.Kansas$min.sv.avg),
         count = n(),
         Tr = ((count + 1)/rank),
         Prob = (1/Tr))

#determining 7Q10 value based off of other values in the table

#plot of minimum 7 day average flow over time
sv.avg.plot <- ggplot(Q.7.avg.prob, aes(x = Year, y = min.sv.avg)) +
  geom_line() +
  labs(x = "Year", y = "Minimum of 7 Day Average Flow (cfs)")
print(sv.avg.plot)

#plot of recurrence interval
recurr.plot <- ggplot(Q.7.avg.prob, aes(x = Tr, y = min.sv.avg)) +
  geom_point() +
  labs(x = "Recurrence Interval", y = "Minimum of 7 Day Average Flow (cfs)")
print(recurr.plot)


#### 7Q10 Analysis for site Missouri River at Hermann, MO, # 06934500  ####

#filtering dataframe to get 7 day average for  06934500 
avg.dis.Missouri <- site.dis %>%
  filter(site_no == "06934500") %>%
  select(site_no, Date, Flow) %>%
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date),
         rM = rollmean(Flow, 7, na.pad = TRUE, align = "right")) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(min(rM))

#renaming second column
names(avg.dis.Missouri)[2] <- c("min.sv.avg")

#mutating columns to include a rank column
Q.7.avg.prob.Missouri <- avg.dis.Missouri %>%
  arrange(avg.dis.Missouri$min.sv.avg) %>%
  mutate(rank = 1:length(avg.dis.Missouri$min.sv.avg),
         count = n(),
         Tr = ((count + 1)/rank),
         Prob = (1/Tr))

#determining 7Q10 value based off of other values in the table

#plot of minimum 7 day average flow over time
sv.avg.plot <- ggplot(Q.7.avg.prob, aes(x = Year, y = min.sv.avg)) +
  geom_line() +
  labs(x = "Year", y = "Minimum of 7 Day Average Flow (cfs)")
print(sv.avg.plot)

#plot of recurrence interval
recurr.plot <- ggplot(Q.7.avg.prob, aes(x = Tr, y = min.sv.avg)) +
  geom_point() +
  labs(x = "Recurrence Interval", y = "Minimum of 7 Day Average Flow (cfs)")
print(recurr.plot)

#### 7Q10 Analysis for site Mill C at Johnson Drive, Shawnee, KS, # 06892513 #### 

#filtering dataframe to get 7 day average for 06892513 
avg.dis.Mill <- site.dis %>%
  filter(site_no == "06892513") %>%
  select(site_no, Date, Flow) %>%
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date),
         rM = rollmean(Flow, 7, na.pad = TRUE, align = "right")) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(min(rM))

#renaming second column
names(avg.dis.Mill)[2] <- c("min.sv.avg")

#mutating columns to include a rank column
Q.7.avg.prob.Mill <- avg.dis.Mill %>%
  arrange(min.sv.avg) %>%
  mutate(rank = 1:length(min.sv.avg),
         count = n(),
         Tr = ((count + 1)/rank),
         Prob = (1/Tr))

#determining 7Q10 value based off of other values in the table

#plot of minimum 7 day average flow over time
sv.avg.plot <- ggplot(Q.7.avg.prob, aes(x = Year, y = min.sv.avg)) +
  geom_line() +
  labs(x = "Year", y = "Minimum of 7 Day Average Flow (cfs)")
print(sv.avg.plot)

#plot of recurrence interval
recurr.plot <- ggplot(Q.7.avg.prob, aes(x = Tr, y = min.sv.avg)) +
  geom_point() +
  labs(x = "Recurrence Interval", y = "Minimum of 7 Day Average Flow (cfs)")
print(recurr.plot)
#### 7Q10 Analysis for site Grand River, Sumner MO  | 06902000 ####

#filtering dataframe to get 7 day average for 06902000
avg.dis.Grand <- site.dis %>%
  filter(site_no == "06902000") %>%
  select(site_no, Date, Flow) %>%
  mutate(Year = year(Date),
         Month = month(Date), 
         Day = day(Date),
         rM = rollmean(Flow, 7, na.pad = TRUE, align = "right")) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(min(rM))

#renaming second column
names(avg.dis.Grand)[2] <- c("min.sv.avg")

#mutating columns to include a rank column
Q.7.avg.prob.Grand <- avg.dis.Grand %>%
  arrange(min.sv.avg) %>%
  mutate(rank = 1:length(min.sv.avg),
         count = n(),
         Tr = ((count + 1)/rank),
         Prob = (1/Tr))

#determining 7Q10 value based off of other values in the table

#plot of minimum 7 day average flow over time
sv.avg.plot <- ggplot(Q.7.avg.prob, aes(x = Year, y = min.sv.avg)) +
  geom_line() +
  labs(x = "Year", y = "Minimum of 7 Day Average Flow (cfs)")
print(sv.avg.plot)

#plot of recurrence interval
recurr.plot <- ggplot(Q.7.avg.prob, aes(x = Tr, y = min.sv.avg)) +
  geom_point() +
  labs(x = "Recurrence Interval", y = "Minimum of 7 Day Average Flow (cfs)")
print(recurr.plot)