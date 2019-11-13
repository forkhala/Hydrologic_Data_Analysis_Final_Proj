getwd()
library(tidyverse)
library(dataRetrieval)
library(dplyr)
library(xts)
library(dygraphs)
library(lubridate)
library(kableExtra)
library(gridExtra)

#setting theme
theme_set(theme_classic())

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

#read in data
bestsites.data <- read.csv("./Data/Raw/bestsites.DNP.csv", colClasses=c("parm_cd"="character"))

#class of Date column
class(bestsites.data$Date)

#changing Date column to class of date
bestsites.data$Date <- as.Date(bestsites.data$Date, "%Y/%m/%d")

class(bestsites.data$Date)

#changing class type for site_no column
bestsites.data$site_no <- as.character(bestsites.data$site_no)
class(bestsites.data$site_no)

#reading in daily discharge values for best sites in each huc
bestsites.discharge <- readNWISdv(siteNumbers = c(best.sites),
                                  parameterCd = "00060", #discharge
                                  startDate = "",
                                  endDate = "")
#names(bestsites.discharge)[4:5] <- c("Discharge", "Approval Code")

#adding column with parameter code for discharge
bestsites.discharge$parm_cd <- "00060"

#Nitrogen, Phosphorus, pH, and total coliform for best sites in each site
bestsites.wq <- readNWISqw(siteNumbers = c(best.sites),
                           parameterCd = c("00600", #TN
                                           "00665", #TP 
                                           "00400", #pH
                                           "31501"), #total coliform
                           startDate = "",
                           endDate = "")

names(bestsites.wq)[3] <- c("Date")

#joinining datatables for water quality and discharge data
bestsites.WQ <- full_join(bestsites.discharge, bestsites.wq,
                           by = c("site_no", "agency_cd", "Date", "parm_cd"))

#saving best site waterquality data as a .csv
write.csv(bestsites.WQ, "./Data/Raw/bestsites.WQ.csv")

#reading in csv file with water quality data
sites.wq <- read.csv("./Data/Raw/bestsites.WQ.csv", colClasses=c("parm_cd"="character", 
                                                            "Date"="Date",
                                                            "site_no"="character"))

#looking at the class of the Date column
class(sites.wq$Date)

#structure of water quality dataframe
waterquality.summary <- summary(bestsites.wq)

#summary of data structure
kable(waterquality.summary,
      caption = "Summary of Water Quality Data in the
      Missouri River Basin") %>% 
  kable_styling(latex_options = c("hold_position", "striped", 
                                  "scale_down")) %>% 
  kableExtra::landscape() 

# ---Exploratory data analysis ----

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

#plotting pH of different sites; color by site
pH.plot <- ggplot(bestsites.wq.skinny, aes(x = Year, y = pH, color = Site)) +
  geom_point(alpha = 0.5) +
  ggtitle("pH of Sites in Missiouri River Basin") +
  labs(x = "Year", y = "pH") +
  ylim(c(5.0, 10))
print(pH.plot)

#violin plot of ph; should we take out the pH that is 0.1 because that is not a possible value?
ph.violin.plot <- ggplot(bestsites.wq.skinny, aes(x = Date, y = pH)) +
  geom_violin() +
  facet_wrap(~Site) +
  labs(x = "Date", y = "pH")
print(ph.violin.plot)

#plot of TN over time
TN.plot <- ggplot(bestsites.wq.skinny, 
                  aes(x = Date, y = total.nitrogen)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Date", y = "Total Nitrogen (mg/L)")
print(TN.plot)

#plot of TN over time facet by site
TN.plot.site <- ggplot(bestsites.wq.skinny, 
                       aes(x = Date, y = total.nitrogen)) +
  geom_violin() +
  facet_wrap(~Site) +
  labs(x = "Date", y = "Total Nitrogen (mg/L)")
print(TN.plot.site)

#plot of TP over time facet by site
TP.plot.site <- ggplot(bestsites.wq.skinny %>% filter(Date > 1988-01-01), 
                                   aes(x = Date, y = total.phosphorus)) +
  geom_violin() +
  facet_wrap(~Site) +
  labs(x = "Date", y = "Total Phosphorus (mg/L)")
print(TP.plot.site)

#selecting sites with only total coliform data
bestsites.tc <- bestsites.wq.skinny %>%
   filter(Site == "06775900" | Site == "06810000" | Site == "06856600" |
            Site == "06902000" | Site == "06934500" | Site == "06905500")
  
#plot of total coliform by date
TC.plot <- ggplot(bestsites.wq.skinny, aes(x = Date, y = total.coliform)) +
  geom_violin() +
  facet_wrap(~Site) +
  labs(x = "Date", y = "Total Coliform (cfu/100 ml)")
print(TC.plot)

#plot of TP with 3 sites that have total coliform, facet by site
TP.plot.2 <- ggplot(bestsites.wq.skinny, aes(x = Date, y = total.phosphorus)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site) +
  xlim(as.Date(c("1969-01-01", "1980-01-01"))) +
  theme_bw() +
  labs(x = "Date", y = "Total Phosphorus (mg/L)")
print(TP.plot.2)

#plot of TP over time with 3 sites that have total coliform
TP.plot <- ggplot(bestsites.wq.skinny, 
                       aes(x = Date, y = total.phosphorus)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Date", y = "Total Phosphorus (mg/L)") +
  xlim(as.Date(c("1969-01-01", "2020-01-01"))) +
  theme(axis.title.x=element_blank())
print(TP.plot)

#grid arrange for TP graphs
grid.arrange(TP.plot, TP.plot.2)

#plot of TN with 3 sites that have total coliform
TN.plot.2 <- ggplot(bestsites.wq.skinny, aes(x = Date, y = total.nitrogen)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Date", y = "Total Nitrogen (mg/L)") +
  xlim(as.Date(c("1969-01-01", "2020-01-01"))) +
  theme(axis.title.x=element_blank())
print(TN.plot.2)

#TN over time facet by site
TN.plot.site <- ggplot(bestsites.wq.skinny, 
                       aes(x = Date, y = total.nitrogen)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Site) +
  labs(x = "Date", y = "Total Nitrogen (mg/L)") +
  xlim(as.Date(c("1969-01-01", "2020-01-01"))) +
  theme_bw()
print(TN.plot.site) 

#grid arrange for TN over time
grid.arrange(TN.plot.2, TN.plot.site)

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

#plot of total coliform over time
TC.plot.time <- ggplot(bestsites.wq.skinny,
                  aes(x = Date, y = total.coliform)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Date", y = "Total Coliform (cfu/100 ml)") +
  xlim(as.Date(c("1969-01-01"," 1975-12-31"))) +
  facet_wrap(~Site) +
  theme_set(theme_bw())
print(TC.plot.time)

#plot of total coliform over time
tc.time <- ggplot(bestsites.wq.skinny, aes(x = Date, y = total.coliform)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  xlim(as.Date(c("1969-01-01", "1975-01-01"))) +
  labs(x = "Date", y = "Total Coliform (cfu/100 ml)") +
  theme(axis.title.x=element_blank())
print(tc.time) 

#grid arrange to put the plots on one figure
grid.arrange(tc.time, TC.plot.time, nrow = 2)


#### seasonal trend plot for total coliform ####

#monthly observations of nutrients, discharge, pH and total coliform
monthly.obs <- bestsites.wq.skinny %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  select(-Date) %>%
  summarize_all(funs(sum(!is.na(.))))

#monthly summary of total coliform data
totalcoli.monthly.summaries <- bestsites.tc %>%
  mutate(month = month(Date)) %>%
  group_by(month) %>%
  select(month, total.coliform) %>%
  summarize_all(funs(Median = median(., na.rm = T),
                     quant25 = quantile(., .25, na.rm = T),
                     quant75 = quantile(., .75, na.rm = T)))


#total coliform season plot
totalcoli.seasons.plot <- ggplot(totalcoli.monthly.summaries, aes(x = month)) +
  geom_ribbon(aes(ymin = quant25, ymax = quant75), alpha = 0.3) +
  geom_line(aes(y = Median)) +
  scale_x_continuous(name = "Month",
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(y = "Total coliforms (cfu / 100 ml)") 
print(totalcoli.seasons.plot) #high total coliform amounts in June, a spike in April, and a spike in October; overlay with discharge?

#daily summary of total coliform data; daily summary of total coliform for day of year for all years
totalcoli.daily.summaries <- bestsites.wq.skinny %>%
  mutate(daynum = day(Date)) %>%
  group_by(daynum) %>%
  select(daynum, total.coliform) %>%
  summarize_all(funs(Median = median(., na.rm = T),
                     quant25 = quantile(., .25, na.rm = T),
                     quant75 = quantile(., .75, na.rm = T)))

#daily total coliform plot
totalcoli.daily.plot <- ggplot(totalcoli.daily.summaries, aes(x = daynum)) +
  geom_ribbon(aes(ymin = quant25, ymax = quant75), alpha = 0.3) +
  geom_line(aes(y = Median)) +
  scale_x_continuous(name = "Day of Year") + 
  labs(y = "Total coliforms (cfu / 100 ml)")
print(totalcoli.daily.plot)


#### TN, TP, pH, TC graphs ####

#discharge and TN plot over time
#plot of discharge and TN over time (dygraph)
Total_Nitrogen <- with(bestsites.wq.skinny, xts(x = total.nitrogen, order.by = Date))
Discharge <- with(bestsites.wq.skinny, xts(x = Discharge, order.by = Date))

DyData.TN <- cbind(Total_Nitrogen, Discharge)

#filtering to include only data from 

dygraph(DyData.TN) %>% 
  dySeries("Total_Nitrogen", axis = "y2") %>% 
  dyAxis(name = "y", label = "Discharge (cfs)") %>%
  dyAxis(name = "y2", label = "Total Nitrogen (mg/l)") %>%
  dyRangeSelector()

#discharge and TP plot over time
Total_Phosphorus <- with(bestsites.wq.skinny, xts(x = total.phosphorus, order.by = Date))
Discharge <- with(bestsites.wq.skinny, xts(x = Discharge, order.by = Date))

DyData.TP <- cbind(Total_Phosphorus, Discharge)

#plotting dygraph with discharge and TP over time
dygraph(DyData.TP) %>% 
  dySeries("Total_Phosphorus", axis = "y2") %>% 
  dyAxis(name = "y", label = "Discharge (cfs)") %>%
  dyAxis(name = "y2", label = "Total Phosphorus (mg/l)") %>%
  dyRangeSelector()

#ggplot of discharge and TN
discharge.TN <- ggplot(bestsites.wq.skinny, aes(x = Date)) +
  geom_line(aes(y = Discharge, color = "blue")) +
  geom_line(aes(y = total.nitrogen, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(name = "Total N (mg/l)")) +
  xlim(c("1970-01-01", "2010-01-01"))
print(discharge.TN)

#discharge and pH plot over time
pH <- with(bestsites.wq.skinny, xts(x = pH, order.by = Date))
Discharge <- with(bestsites.wq.skinny, xts(x = Discharge, order.by = Date))

DyData.ph <- cbind(pH, Discharge)

#plotting dygraph with discharge and TP over time
dygraph(DyData.ph) %>% 
  dySeries("pH", axis = "y2") %>% 
  dyAxis(name = "y", label = "Discharge (cfs)") %>%
  dyAxis(name = "y2", label = "pH") %>%
  dyRangeSelector()

#plot of discharge and TC over time (dygraph)
Total_Coliform <- with(bestsites.tc, xts(x = total.coliform, order.by = Date))
Discharge <- with(bestsites.wq.skinny, xts(x = Discharge, order.by = Date))

DyData <- cbind(Total_Coliform, Discharge)

dygraph(DyData) %>% 
  dySeries("Total_Coliform", axis = "y2") %>% 
  dyAxis(name = "y", label = "Discharge (cfs)") %>%
  dyAxis(name = "y2", label = "Total Coliform (cfu per 100 ml)", valueRange = c(0, 600000)) %>%
  dyRangeSelector()

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
