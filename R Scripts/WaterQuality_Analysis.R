getwd()
library(tidyverse)
library(dataRetrieval)
library(dplyr)
library(xts)
library(dygraphs)
library(lubridate)
library(kableExtra)
library(gridExtra)
#install.packages("data.table")
library(data.table)
#install.packages("dtplyr")
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

#setting theme
theme_set(theme_classic())

#### Reading in data and wrangling ####

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

#saving best site waterquality data as a .csv
write.csv(bestsites.WQ, "./Data/Raw/bestsites.WQ.csv")

#reading in csv file with water quality data
sites.wq <- read.csv("./Data/Raw/bestsites.WQ.csv", colClasses=c("parm_cd"="character", 
                                                                 "Date"="Date",
                                                                 "site_no"="character"))

#looking at the class of the Date column
class(sites.wq$Date)


#joining bestsites and bacteria data
bestsites.bac.dis <- bestsites.discharge %>%
  full_join(., bestsites.bacteria,
            by = c("site_no", "agency_cd", "Date", "parm_cd"))

bestsites.bac.dis.names <- left_join(bestsites.bac.dis, site.list,
                                     by = c("site_no"))

#filtering dataset 
bestsites.wq.skinny <- sites.wq %>%
  select(Site = site_no,
         Date = Date,
         Parameter = parm_cd,
         Value = result_va,
         Discharge = X_00060_00003) %>%
  group_by(Date, Parameter, Site) %>%
  summarize(Value = mean(Value),
            Discharge = mean(Discharge)) %>%
  spread(key = Parameter, value = Value) %>% 
  rename(pH = '00400', total.coliform = '31501', 
         Discharge2 = '00060', total.nitrogen = '00600', 
         total.phosphorus = '00665') %>%
  mutate(Year = year(Date)) %>%
  select(-Discharge2) %>%     
  filter(Site == "06810000" | Site == "06856600" |
           Site == "06934500")

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
  summarize(Value = mean(Value) %>%
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

#### Exploratory Data Analysis ####

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
