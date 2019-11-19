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

#all bestsites for the hucs
bestsites1021.1026.1027.1030 <- 
  c("06775900", "06794000", "06877600", "06874000", "06892350", 
    "06887500", "06934500", "06894000")
bestsites1024.1025 <- c("06844500", "06856600", "06818000", "06810000")
bestsites1020.1023 <- c("06768000", "06805500", "06775900", "06794000", 
                        "06800000", "06800500", "06609500", "06610000")
bestsites1028.1029 <- c("06902000", "06905500", "06921070", "06926510")

#putting all best sites into one vector
best.sites <- c(bestsites1021.1026.1027.1030, bestsites1024.1025, 
                bestsites1020.1023, bestsites1028.1029)
best.sites <- unique(best.sites)

#reading in file with site list and names
site.list <- read.csv("./Data/Processed/bestsiteslist.csv", 
                      colClasses=c("site_no"="character"))

#reading in daily discharge values for best sites in each huc
bestsites.discharge <- readNWISdv(siteNumbers = c(best.sites),
                                  parameterCd = "00060", #discharge
                                  startDate = "",
                                  endDate = "")

#adding column with parameter code for discharge
bestsites.discharge$parm_cd <- "00060"

#reading in all bacteria data
bestsites.bacteria <- readNWISqw(siteNumbers = c(best.sites),
                                 parameterCd = c("95200",  #Total cell count
                                                 "31501",  #Total coliforms
                                                 "31625",  #Fecal coliforms (0.7 um filter)
                                                 "31616",  #Fecal coliforms (0.45 um filter)
                                                  "31673",  #Fecal streptocci KF streptocaccus
                                                  "31679",  #Fecal streptocci m-Enterococcus
                                                 "00600", #TN
                                                 "00665", #TP 
                                                 "00400"), #pH
                                 startDate = "",
                                 endDate = "")

names(bestsites.bacteria)[3] <- c("Date")

#joining bestsites and bacteria data
bestsites.bac.dis <- bestsites.discharge %>%
   full_join(., bestsites.bacteria,
                  by = c("site_no", "agency_cd", "Date", "parm_cd"))

bestsites.bac.dis.names <- left_join(bestsites.bac.dis, site.list,
                                     by = c("site_no"))

# ---Exploratory data analysis ----

#writing processed data file for water quality
write.csv(bestsites.wq.skinny, "./Data/Processed/waterquality_processed.csv")

#bestsites dis and bacteria
bestsites.bac.dis.skinny <- bestsites.bac.dis.names %>%
  select(Site = site_no,
         Date = Date,
         Parameter = parm_cd,
         Value = result_va,
         Discharge = X_00060_00003,
         Site.Name = site_nm,
         Huc.Name = huc4_nm) %>%
  group_by(Date, Parameter, Site, Site.Name) %>%
  summarize(Value = mean(Value),
            Discharge = mean(Discharge)) %>%
  spread(key = Parameter, value = Value) %>% 
  rename(pH = '00400', total.coliform = '31501', 
         Discharge2 = '00060', total.nitrogen = '00600', 
         total.phosphorus = '00665', total.cell = '95200',
         total.coli = '31501', F.coli.7 = '31625', F.coli.45 = '31616',
         F.strep.strep = '31673', F.strep.ente = '31679') %>%
  mutate(Year = year(Date), 
         Month = month(Date)) %>%
  select(-Discharge2) 


#plotting pH of different sites; color by site
pH.plot <- ggplot(bestsites.bac.dis.skinny, aes(x = Date, y = pH, color = Site.Name)) +
  geom_point(alpha = 0.5) +
  ggtitle("pH of Sites in Missiouri River Basin") +
  labs(x = "Date", y = "pH") +
  ylim(c(5.0, 10))
print(pH.plot) #pH disappears when I just do it by month

#coliform plots
coli.plot <- ggplot(bestsites.bac.dis.skinny, aes(x = Date, y = F.coli.7,
                                                  color = Site.Name)) +
  geom_point()
print(coli.plot)

coli.plot.violin.7 <- ggplot(bestsites.bac.dis.skinny, aes(x = Year, y = F.coli.7)) +
  geom_violin() +
  facet_wrap(~Site.Name)
print(coli.plot.violin.7) #only 8 out of 22 sites have this data for 2019

coli.plot.strep <- ggplot(bestsites.bac.dis.skinny, aes(x = Date, y = F.strep.strep)) +
  geom_violin() +
  facet_wrap(~Site.Name)
print(coli.plot.strep) #not many data over time

coli.plot.45 <- ggplot(bestsites.bac.dis.skinny, aes(x = Date, y = F.coli.45)) +
  geom_violin() +
  facet_wrap(~Site.Name)
print(coli.plot.45) #not much data over time

coli.plot.ente <- ggplot(bestsites.bac.dis.skinny, aes(x = Date, y = F.strep.ente)) +
                                                      
  geom_violin() +
  facet_wrap(~Site.Name)
print(coli.plot.ente) #not much over time; sampling stops in 1978

#selecting sites with F.Coli 0.7 data overtime 
bestsites.coli <- bestsites.bac.dis.skinny %>%
   filter(Site == "06921070" | Site == "06926510" | Site == "06902000" | 
            Site == "06934500" | Site == "06905500")
  
#plot of total coliform by date
F.coli.7.plot <- ggplot(bestsites.coli, aes(x = Date, y = F.coli.7)) +
  geom_violin() +
  facet_wrap(~Site.Name) +
  labs(x = "Date",
       y = expression("Fecal coliforms filtered to 0.7 " * mu *m *  " (cfu / 100 ml)"))
print(F.coli.7.plot)

#plot of F.coli.7 over time 
F.coli.7.time.plot <- ggplot(bestsites.coli, 
                       aes(x = Date, y = F.coli.7)) +
  geom_point() +
  labs(x = "Date", 
       y = expression("Fecal coliforms filtered to 0.7 " * mu *m *  " (cfu / 100 ml)")) +
  xlim(as.Date(c("1975-01-01", "2020-01-01")))
print(F.coli.7.time.plot)

#plot of fecal coliform 0.7 facet by site
F.coli.7.site.plot <- ggplot(bestsites.coli,
                             aes(x = Date, y = F.coli.7)) +
  geom_point() +
  labs(x = "Date", 
       y = expression("Fecal coliforms filtered to 0.7 "* mu *m *" (cfu / 100 ml)")) +
  xlim(as.Date(c("1975-01-01", "2020-01-01"))) +
  facet_wrap(~Site.Name)
print(F.coli.7.site.plot)


#grid arrange for F.coli.7  over time
grid.arrange(F.coli.7.time.plot, F.coli.7.site.plot)

#plot of pH with 3 sites that have total coliform
ph.plot.2 <- ggplot(bestsites.wq.skinny, aes(x = Date, y = pH)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site) +
  xlim(as.Date(c("1960-01-01", "2020-01-01")))
print(ph.plot.2)

#plot of pH in 3 sites with total coliform data over time
ph.plot <- ggplot(bestsites.wq.skinny, aes(x = Date, y = pH)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(as.Date(c("1960-01-01", "2020-01-01"))) +
  theme(axis.title.x=element_blank())
print(ph.plot)  

#grid arrange for pH plot
grid.arrange(ph.plot, ph.plot.2)

#### seasonal trend plot for fecal coliform ####

#monthly observations Fecal coliform (0.7)
monthly.obs <- bestsites.coli %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  select(-Date) %>%
  summarize_all(funs(sum(!is.na(.))))

#monthly summary of Fecal Coliform (0.7)
F.coli.7.monthly.summaries <- bestsites.coli %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  select(month, F.coli.7) %>%
  summarize_all(funs(Median = median(., na.rm = T),
                     quant25 = quantile(., .25, na.rm = T),
                     quant75 = quantile(., .75, na.rm = T)))


#Fecal Coliform (0.7) season plot
F.coli.7.seasons.plot <- ggplot(F.coli.7.monthly.summaries, aes(x = month)) +
  geom_ribbon(aes(ymin = quant25, ymax = quant75), alpha = 0.3) +
  geom_line(aes(y = Median)) +
  scale_x_continuous(name = "Month",
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(y = expression("Fecal coliforms filtered to 0.7 " * mu *m *  " (cfu / 100 ml)")) 
print(F.coli.7.seasons.plot) #high total coliform amounts in June, a spike in April, and a spike in October; overlay with discharge?

#daily summary of total coliform data; daily summary of total coliform for day of year for all years
F.coli.7.daily.summaries <- bestsites.coli %>%
  mutate(daynum = day(Date)) %>%
  group_by(daynum) %>%
  select(daynum, F.coli.7) %>%
  summarize_all(funs(Median = median(., na.rm = T),
                     quant25 = quantile(., .25, na.rm = T),
                     quant75 = quantile(., .75, na.rm = T)))

#daily fecal coliform 0.7 plot
F.coli.7.daily.plot <- ggplot(F.coli.7.daily.summaries, aes(x = daynum)) +
  geom_ribbon(aes(ymin = quant25, ymax = quant75), alpha = 0.3) +
  geom_line(aes(y = Median)) +
  scale_x_continuous(name = "Day of Year") + 
  labs(y = expression("Fecal coliforms filtered to 0.7 " * mu *m *  " (cfu / 100 ml)"))
print(F.coli.7.daily.plot)

#### Time Series Analysis ####

###Grand River Time Series###

#filter to only get Grand River site
grand.river.data <- bestsites.coli %>%
  select(Date, Site, Site.Name, F.coli.7, 
         Year, Month) %>%
  filter(Site == '06902000') %>%
  na.omit()

#seeing if linear regression makes sense for this timeseries
GrandRiverRegressionPlot <-
  ggplot(grand.river.data, aes(x = Date, y = F.coli.7)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "Date", y = expression("Fecal coliforms filtered to 0.7 " * mu *m *  " (cfu / 100 ml)")) +
  theme(plot.title = element_text(margin = margin(b = -10), size = 12)) +
  xlim(as.Date(c("1975-01-01", "2020-01-01")))
print(GrandRiverRegressionPlot) #looks like a negative trend

#creating time series of data
grand.river_ts <- ts(grand.river.data[[4]], frequency = 24)

#decomposing timeseries data using 'stl'
grand.river_decomposed <- stl(grand.river_ts, s.window = "periodic")

#plotting decomposed time series
plot(grand.river.data)

#### Linear Models ####

#plot of total coliform over time
totalcoliform.time <- ggplot(bestsites.wq.skinny, aes(x = Date)) +
  geom_line(aes(y = Discharge, alpha = 0.5), color = "black") +
  geom_point(aes(y = total.coliform), color = "red") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1.20, name = "Discharge (cfs)")) +
  xlim(as.Date(c("1969-01-01", "1980-01-01"))) +
  labs(x = "Date", y = "Total Coliform (cfu/100 ml)")
print(totalcoliform.time)

#plot of TN over time and linear regression line

tn.mod <- lm(data = bestsites.wq.skinny, total.nitrogen ~ Date)
summary(tn.mod) #for every day increase, total nitrogen increases by 7.321e-05 mg/l. Thus there is a significant increase over the time period (p < 2e-16, F-statistic = 74.82).

tn.time <- ggplot(bestsites.tc, aes(x = Date, y = total.nitrogen)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  xlim(as.Date(c("1965-01-01", "2019-01-01")))
print(tn.time)

#linear model of total coliform over time
tc.mod <- lm(data = bestsites.tc, total.coliform ~ Date)
summary(tc.mod)

#plot of total coliform over time
tc.time <- ggplot(bestsites.tc, aes(x = Date, y = total.coliform)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  xlim(as.Date(c("1969-01-01", "1980-01-01"))) +
  labs(x = "Date", y = "Total Coliform (cfu/100 ml)")
print(tc.time) 

#linear model of total phosphorus over time with just 6 sites that have total coliform
tp.mod <- lm(data = bestsites.tc, total.phosphorus ~ Date)
summary(tp.mod)

#tp model with all 22 sites
tp.mod.2 <- lm(data = bestsites.wq.skinny, total.phosphorus ~ Date)
summary(tp.mod.2)

#pH model
ph.mod <- lm(data = bestsites.wq.skinny, pH ~ Date)
summary(ph.mod)

#plot of pH over time
ph.time <- ggplot(bestsites.tc, aes(x = Date, y = pH)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  xlim(as.Date(c("1950-01-01", "2020-01-01")))  +
  labs(x = "Date", y = "pH")
print(ph.time) 


#linear model for TN 
tn.mod <- lm(data = bestsites.wq.skinny, total.nitrogen ~ Date)
summary(tn.mod) #for every day increase, total nitrogen increases by 9.310e-05 mg/l. Thus there is a significant increase over the time period (p < 2e-16, F-statistic = 23.31).

tn.mod.2 <- lm(data = bestsites.wq.skinny, Date ~ total.nitrogen)
summary(tn.mod.2)

#linear model of total coliform over time
tc.mod <- lm(data = bestsites.wq.skinny, total.coliform ~ Date)
summary(tc.mod) #for every day increase, total coliform increases by 1.046 cfu/100 ml. Thus, there is an increase over the time period (p < 0.0138, F-statistic = 0.01576).

#linear model of total phosphorus over time with just 6 sites that have total coliform
tp.mod <- lm(data = bestsites.wq.skinny, total.phosphorus ~ Date)
summary(tp.mod) #for every day increase, total phosphorus increases by 1.887e-05 mg/L. Thus, there is a significant increase in total phosphorus over the time period (p < 2.14e-15, F-statistic = 30.92).

#pH model
ph.mod <- lm(data = bestsites.wq.skinny, pH ~ Date)
summary(ph.mod) #for every day increase

#Linear models were run for total nitrogen, total phosphorus, total coliform, and pH to determine if there were significant patterns in the data.


#compiling site numbers together
sites <- c("06808500", "06817000", "06892350", "06934500", "06892513", "06902000")

#reading in data for sites that were analyzed for floods
site.dis <- readNWISdv(siteNumbers = c(sites),
                         parameterCd = "00060", #discharge
                         startDate = "",
                         endDate = "") %>%
  renameNWISColumns()


####Exploratory data analysis (droughts)####

#reading in file with site list and names
site.list <- read.csv("./Data/Processed/bestsiteslist.csv", 
                      colClasses=c("site_no"="character"))

#all bestsites for the hucs
bestsites1021.1026.1027.1030 <- 
  c("06775900", "06794000", "06877600", "06874000", "06892350", 
    "06887500", "06934500", "06894000")
bestsites1024.1025 <- c("06844500", "06856600", "06818000", "06810000")
bestsites1020.1023 <- c("06768000", "06805500", "06775900", "06794000", 
                        "06800000", "06800500", "06609500", "06610000")
bestsites1028.1029 <- c("06902000", "06905500", "06921070", "06926510")

#putting all best sites into one vector
best.sites <- c(bestsites1021.1026.1027.1030, bestsites1024.1025, 
                bestsites1020.1023, bestsites1028.1029)
best.sites <- unique(best.sites)

#reading in daily discharge values for best sites in each huc
bestsites.discharge <- readNWISdv(siteNumbers = c(best.sites),
                                  parameterCd = "00060", #discharge
                                  startDate = "",
                                  endDate = "")

#renaming columns
bestsites.discharge <- renameNWISColumns(bestsites.discharge)

#reading in site info for 06808500
stationInfo <- readNWISsite(siteNumber = "06808500")

#filtering for just one site 
site.1.dis <- site.dis %>%
  filter(site_no == "06808500")

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
  mutate(rM = rollmean(Flow, 7, na.pad = TRUE, align = "right"))

#creating a dataframe with just flow data and   
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

#### 7Q10 Analysis for site Kansas River in Desoto, KS, # 06892350 
#### 7Q10 Analysis for site Missouri River at Hermann, MO, # 6934500  
#### 7Q10 Analysis for site Mill C at Johnson Drive, Shawnee, KS, # 06892513 
#### 7Q10 Analysis for site Grand River, Sumner MO  | 06902000 
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



