#Flooding analysis

#article about 2019 floods: https://www.kansascity.com/news/state/missouri/article228237519.html
#has good picture of where levees broke

pacman::p_load(tidyverse, dataRetrieval, dygraphs, maps, sf, xts)

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
                               startDate = "2019-01-01",
                               endDate = "2019-11-01") %>%
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
print(Hermann.plot) #no pattern of concentration over discharge - chemostatic

Desoto.plot <- ggplot(Desoto,
               aes(x = Flow_Inst, y = Nitrate_mgl)) +
               geom_point() +
               scale_x_log10() +
               scale_y_log10()
print(Desoto.plot) #nitrate concentration barely changes with change in flow, chemostatic
#conc not dependent on discharge fluxes (not coming from overland flow?)

Clarinda.plot <- ggplot(Clarinda,
                 aes(x = Flow_Inst, y = Nitrate_mgl)) +
                 geom_point() +
                 scale_x_log10() +
                 scale_y_log10()
print(Clarinda.plot) #no discernable patter, huge variation in N conc within 
#small changes of discharge (influenced by season?)

Randolph.plot <- ggplot(Randolph,
                 aes(x = Flow_Inst, y = Nitrate_mgl)) +
                 geom_point() +
                 scale_x_log10() +
                 scale_y_log10()
print(Randolph.plot) #chemostatic no discernable pattern whatsoever


#Chemostatic: Randolph, Clarinda, and Hermann are all chemostatic.
#Desoto is also chemostatic, but very little overall change in N conc


#------------Dygraph to determine flushing or diluting behavior---------#

dygraph(
  cbind(
    Flow = xts(Hermann$Flow_Inst, order.by = Hermann$dateTime), 
    Nitrate = xts(Hermann$Nitrate_mgl, order.by = Hermann$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

dygraph(
  cbind(
    Flow = xts(Clarinda$Flow_Inst, order.by = Clarinda$dateTime), 
    Nitrate = xts(Clarinda$Nitrate_mgl, order.by = Clarinda$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

dygraph(
  cbind(
    Flow = xts(Desoto$Flow_Inst, order.by = Desoto$dateTime), 
    Nitrate = xts(Desoto$Nitrate_mgl, order.by = Desoto$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
#desoto looks like more of a diluting river (end of June)

dygraph(
  cbind(
    Flow = xts(Randolph$Flow_Inst, order.by = Randolph$dateTime), 
    Nitrate = xts(Randolph$Nitrate_mgl, order.by = Randolph$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()

#desoto storm
DesotoStorm <- Desoto %>%
  filter(dateTime > "2019-02-22" & dateTime < "2019-02-28") 


ggplot(DesotoStorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() 
#very loose counter clockwise motion - loose positive slope for flushing storm

#Clarinda storm
ClarindaStorm <- Clarinda %>%
  filter(dateTime > "2019-06-24" & dateTime < "2019-06-30") 


ggplot(ClarindaStorm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() 

#----------baseflow separation--------------#

#----Hermann-----#
#first, interpolate missing flow values
Hermann.skinny <- Hermann %>% select(dateTime, Flow_Inst)
table(diff(Hermann.skinny$dateTime))
as.Date("2019-01-01")-as.Date("2019-11-01")
#so timestep = 304 days*96 (number time steps in a day (4*24)) = 29184

#interpolate by number of days in time period
linearinterpolation <- as.data.frame(approx(Hermann.skinny, n = 29184, method = "linear"))
linearinterpolation$x <- as.POSIXct(linearinterpolation$x, origin = "1970-01-01")
names(linearinterpolation) <- c("dateTime", "Flow_Inst")

#plot interpolated data onto full data to see if it looks right
Hermann.interpolate.plot <- 
  ggplot(Hermann.skinny, aes(x = dateTime, y = Flow_Inst)) +
  geom_line() +
  geom_point(data = linearinterpolation, aes(x = dateTime, y = Flow_Inst), color = "#c13d75ff") 
#print(Hermann.interpolate.plot)

Hermann.baseflow <- EcoHydRology::BaseflowSeparation(
  linearinterpolation$Flow_Inst, 
  filter_parameter = 0.925, #default parameter
  passes = 3 #default parameter
)
#gives us two columns: bt(baseflow time series) and qft (quickflow time series)

Hermann.full <- cbind(linearinterpolation, Hermann.baseflow)

Hermann.full.plot <- ggplot(Hermann.full, aes(x = dateTime)) + 
  geom_line(aes(y = Flow_Inst, color="Total"), size=1.1) +
  geom_line(mapping = aes(y = bt, color = "Baseflow"), size = 1.01) +
  geom_line(mapping = aes(y = qft, color="Quickflow"), size = 1.01) +
  labs(x="Date", y=expression("Discharge (m"^3*"/s)"), color="Flow type") +
  scale_colour_manual(values = c(
    'Total' = 'black',
    'Baseflow' = 'darkcyan',
    'Quickflow' = 'coral'))

print(Hermann.full.plot)

Hermann.Export <- Hermann.full %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_), #what is time difference in between each of these time stamps? dateTime is in POSIXct format. as.numeric changes it to the number of seconds between two time stamps. added NA value added at end because a difference gives us one less point.
         baseflowexport = bt * timestep, #volume = rate (bt) x time
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T), #sum of all volume over the year
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

Hermann.Export$BaseflowExport_cf/Hermann.Export$TotalExport_cf
#98.9% as baseflow
1-(Hermann.Export$BaseflowExport_cf/Hermann.Export$TotalExport_cf)
#1.1% as quickflow

#----Clarinda-----#

#first, interpolate missing flow values
Clarinda.skinny <- Clarinda %>% select(dateTime, Flow_Inst)
table(diff(Clarinda.skinny$dateTime))
as.Date("2019-01-01")-as.Date("2019-11-01")
#so timestep = 304 days*96 (number time steps in a day (4*24)) = 29184

#interpolate by number of days in time period
linearinterpolation <- as.data.frame(approx(Clarinda.skinny, n = 29184, method = "linear"))
linearinterpolation$x <- as.POSIXct(linearinterpolation$x, origin = "1970-01-01")
names(linearinterpolation) <- c("dateTime", "Flow_Inst")

#plot interpolated data onto full data to see if it looks right
Clarinda.interpolate.plot <- 
  ggplot(Clarinda.skinny, aes(x = dateTime, y = Flow_Inst)) +
  geom_line() +
  geom_point(data = linearinterpolation, aes(x = dateTime, y = Flow_Inst), color = "#c13d75ff") 
#print(Clarinda.interpolate.plot)

Clarinda.baseflow <- EcoHydRology::BaseflowSeparation(
  linearinterpolation$Flow_Inst, 
  filter_parameter = 0.925, #default parameter
  passes = 3 #default parameter
)
#gives us two columns: bt(baseflow time series) and qft (quickflow time series)

Clarinda.full <- cbind(linearinterpolation, Clarinda.baseflow)

Clarinda.full.plot <- ggplot(Clarinda.full, aes(x = dateTime)) + 
  geom_line(aes(y = Flow_Inst, color="Total"), size=1.1) +
  geom_line(mapping = aes(y = bt, color = "Baseflow"), size = 1.01) +
  geom_line(mapping = aes(y = qft, color="Quickflow"), size = 1.01) +
  labs(x="Date", y=expression("Discharge (m"^3*"/s)"), color="Flow type") +
  scale_colour_manual(values = c(
    'Total' = 'black',
    'Baseflow' = 'darkcyan',
    'Quickflow' = 'coral'))

print(Clarinda.full.plot)

Clarinda.Export <- Clarinda.full %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_), #what is time difference in between each of these time stamps? dateTime is in POSIXct format. as.numeric changes it to the number of seconds between two time stamps. added NA value added at end because a difference gives us one less point.
         baseflowexport = bt * timestep, #volume = rate (bt) x time
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T), #sum of all volume over the year
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

Clarinda.Export$BaseflowExport_cf/Clarinda.Export$TotalExport_cf
#82.5% as baseflow
1-(Clarinda.Export$BaseflowExport_cf/Clarinda.Export$TotalExport_cf)
#17.5% as quickflow

#----Desoto-----#

#first, interpolate missing flow values
Desoto.skinny <- Desoto %>% select(dateTime, Flow_Inst)
table(diff(Desoto.skinny$dateTime))
as.Date("2019-01-01")-as.Date("2019-11-01")
#so timestep = 304 days*96 (number time steps in a day (4*24)) = 29184

#interpolate by number of days in time period
linearinterpolation <- as.data.frame(approx(Desoto.skinny, n = 29184, method = "linear"))
linearinterpolation$x <- as.POSIXct(linearinterpolation$x, origin = "1970-01-01")
names(linearinterpolation) <- c("dateTime", "Flow_Inst")

#plot interpolated data onto full data to see if it looks right
Desoto.interpolate.plot <- 
  ggplot(Desoto.skinny, aes(x = dateTime, y = Flow_Inst)) +
  geom_line() +
  geom_point(data = linearinterpolation, aes(x = dateTime, y = Flow_Inst), color = "#c13d75ff") 
print(Desoto.interpolate.plot)

Desoto.baseflow <- EcoHydRology::BaseflowSeparation(
  linearinterpolation$Flow_Inst, 
  filter_parameter = 0.925, #default parameter
  passes = 3 #default parameter
)
#gives us two columns: bt(baseflow time series) and qft (quickflow time series)

Desoto.full <- cbind(linearinterpolation, Desoto.baseflow)

Desoto.full.plot <- ggplot(Desoto.full, aes(x = dateTime)) + 
  geom_line(aes(y = Flow_Inst, color="Total"), size=1.1) +
  geom_line(mapping = aes(y = bt, color = "Baseflow"), size = 1.01) +
  geom_line(mapping = aes(y = qft, color="Quickflow"), size = 1.01) +
  labs(x="Date", y=expression("Discharge (m"^3*"/s)"), color="Flow type") +
  scale_colour_manual(values = c(
    'Total' = 'black',
    'Baseflow' = 'darkcyan',
    'Quickflow' = 'coral'))

print(Desoto.full.plot)

Desoto.Export <- Desoto.full %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_), #what is time difference in between each of these time stamps? dateTime is in POSIXct format. as.numeric changes it to the number of seconds between two time stamps. added NA value added at end because a difference gives us one less point.
         baseflowexport = bt * timestep, #volume = rate (bt) x time
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T), #sum of all volume over the year
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

Desoto.Export$BaseflowExport_cf/Desoto.Export$TotalExport_cf
#96.2% as baseflow
1-(Desoto.Export$BaseflowExport_cf/Desoto.Export$TotalExport_cf)
#3.8% as quickflow

#----Randolph-----#

#first, interpolate missing flow values
Randolph.skinny <- Randolph %>% select(dateTime, Flow_Inst)
table(diff(Randolph.skinny$dateTime))
as.Date("2019-01-01")-as.Date("2019-11-01")
#so timestep = 304 days*96 (number time steps in a day (4*24)) = 29184

#interpolate by number of days in time period
linearinterpolation <- as.data.frame(approx(Randolph.skinny, n = 29184, method = "linear"))
linearinterpolation$x <- as.POSIXct(linearinterpolation$x, origin = "1970-01-01")
names(linearinterpolation) <- c("dateTime", "Flow_Inst")

#plot interpolated data onto full data to see if it looks right
Randolph.interpolate.plot <- 
  ggplot(Randolph.skinny, aes(x = dateTime, y = Flow_Inst)) +
  geom_line() +
  geom_point(data = linearinterpolation, aes(x = dateTime, y = Flow_Inst), color = "#c13d75ff") 
print(Randolph.interpolate.plot)

Randolph.baseflow <- EcoHydRology::BaseflowSeparation(
  linearinterpolation$Flow_Inst, 
  filter_parameter = 0.925, #default parameter
  passes = 3 #default parameter
)
#gives us two columns: bt(baseflow time series) and qft (quickflow time series)

Randolph.full <- cbind(linearinterpolation, Randolph.baseflow)

Randolph.full.plot <- ggplot(Randolph.full, aes(x = dateTime)) + 
  geom_line(aes(y = Flow_Inst, color="Total"), size=1.1) +
  geom_line(mapping = aes(y = bt, color = "Baseflow"), size = 1.01) +
  geom_line(mapping = aes(y = qft, color="Quickflow"), size = 1.01) +
  labs(x="Date", y=expression("Discharge (m"^3*"/s)"), color="Flow type") +
  scale_colour_manual(values = c(
    'Total' = 'black',
    'Baseflow' = 'darkcyan',
    'Quickflow' = 'coral'))

print(Randolph.full.plot)

Randolph.Export <- Randolph.full %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_), #what is time difference in between each of these time stamps? dateTime is in POSIXct format. as.numeric changes it to the number of seconds between two time stamps. added NA value added at end because a difference gives us one less point.
         baseflowexport = bt * timestep, #volume = rate (bt) x time
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T), #sum of all volume over the year
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

Randolph.Export$BaseflowExport_cf/Randolph.Export$TotalExport_cf
#94.9% as baseflow
1-(Randolph.Export$BaseflowExport_cf/Randolph.Export$TotalExport_cf)
#5.1% as quickflow

#---try where Randolph is shorted to only look at storm month---#

Randolph.March <- Randolph %>%
  filter(dateTime > "2019-03-01" & dateTime < "2019-03-31") %>%
  select(dateTime, Flow_Inst)

#first, interpolate missing flow values
Randolph.skinny <- Randolph %>% select(dateTime, Flow_Inst)
table(diff(Randolph.skinny$dateTime))
30*96

#interpolate by number of days in time period
linearinterpolation <- as.data.frame(approx(Randolph.March, n = 2880, method = "linear"))
linearinterpolation$x <- as.POSIXct(linearinterpolation$x, origin = "1970-01-01")
names(linearinterpolation) <- c("dateTime", "Flow_Inst")

Randolph.March.baseflow <- EcoHydRology::BaseflowSeparation(
  linearinterpolation$Flow_Inst, 
  filter_parameter = 0.925, #default parameter
  passes = 3 #default parameter
)
#gives us two columns: bt(baseflow time series) and qft (quickflow time series)

Randolph.March.full <- cbind(linearinterpolation, Randolph.March.baseflow)

Randolph.March.full.plot <- ggplot(Randolph.March.full, aes(x = dateTime)) + 
  geom_line(aes(y = Flow_Inst, color="Total"), size=1.1) +
  geom_line(mapping = aes(y = bt, color = "Baseflow"), size = 1.01) +
  geom_line(mapping = aes(y = qft, color="Quickflow"), size = 1.01) +
  labs(x="Date", y=expression("Discharge (m"^3*"/s)"), color="Flow type") +
  scale_colour_manual(values = c(
    'Total' = 'black',
    'Baseflow' = 'darkcyan',
    'Quickflow' = 'coral'))

print(Randolph.March.full.plot)

Randolph.March.Export <- Randolph.March.full %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_), #what is time difference in between each of these time stamps? dateTime is in POSIXct format. as.numeric changes it to the number of seconds between two time stamps. added NA value added at end because a difference gives us one less point.
         baseflowexport = bt * timestep, #volume = rate (bt) x time
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T), #sum of all volume over the year
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

Randolph.March.Export$BaseflowExport_cf/Randolph.March.Export$TotalExport_cf
#91.1% as baseflow
1-(Randolph.March.Export$BaseflowExport_cf/Randolph.March.Export$TotalExport_cf)
#8.9% as quickflow



#------------hydrologic flashiness-------------#
Hermann.sitedata <- readNWISsite(site = "06934500")
Hermann.catchment.size <- Hermann.sitedata$drain_area_va #square miles = 522500

RBI_Hermann <- (
  sum(abs(diff(Hermann.full$Flow_Inst))) / 
    sum(Hermann.full$Flow_Inst[-1])
) / Hermann.catchment.size
#RBI = 2.48e-08, very low

Clarinda.sitedata <- readNWISsite(site = "06817000")
Clarinda.catchment.size <- Clarinda.sitedata$drain_area_va #square miles = 762

RBI_Clarinda <- (
  sum(abs(diff(Clarinda.full$Flow_Inst))) / 
    sum(Clarinda.full$Flow_Inst[-1])
) / Clarinda.catchment.size
#RBI = 1.7e-05, highest flashiness

Desoto.sitedata <- readNWISsite(site = "06892350")
Desoto.catchment.size <- Desoto.sitedata$drain_area_va #square miles = 59756

RBI_Desoto <- (
  sum(abs(diff(Desoto.full$Flow_Inst))) / 
    sum(Desoto.full$Flow_Inst[-1])
) / Desoto.catchment.size
#RBI=5.44e-08, very low

Randolph.sitedata <- readNWISsite(site = "06808500")
Randolph.catchment.size <- Randolph.sitedata$drain_area_va #square miles = 1326

RBI_Randolph <- (
  sum(abs(diff(Randolph.full$Flow_Inst))) / 
    sum(Randolph.full$Flow_Inst[-1])
) / Randolph.catchment.size
#RBI=2.98e-06



################################################################################

#site info with dates of data
highfreqsite2018 <- highfreqsiteinfo %>%
  filter(end_date > "2018-10-01"); head(highfreqsite2018)


#individual sites
Hermann.2018 <- readNWISuv(site="06934500", parameterCd = c("00060", "99133"), 
                           startDate = "2018-01-01",
                           endDate = "2019-01-01") %>%
                            renameNWISColumns() %>%
                            rename(Nitrate_mgl = 6)
Desoto.2018 <- readNWISuv(site="06892350", parameterCd = c("00060", "99133"), 
                          startDate = "2018-01-01",
                          endDate = "2019-01-01") %>%
                          renameNWISColumns() %>%
                          rename(Nitrate_mgl = 6)
Clarinda.2018 <- readNWISuv(site="06817000", parameterCd = c("00060", "99133"), 
                            startDate = "2018-01-01",
                            endDate = "2019-01-01") %>%
                            renameNWISColumns() %>%
                            rename(Nitrate_mgl = 6)
                           
Randolph.2018 <- readNWISuv(site="06808500", parameterCd = c("00060", "99133"), 
                           startDate = "2018-01-01",
                           endDate = "2019-01-01") %>%
                           renameNWISColumns() %>%
                           rename(Nitrate_mgl = 6)
Shawnee.2018 <-  readNWISuv(site="06892513", parameterCd = c("00060", "99133"), 
                            startDate = "2018-01-01",
                            endDate = "2019-01-01") %>%
                            renameNWISColumns() %>%
                            rename(Nitrate_mgl = 6)    
Sumner.2018 <-  readNWISuv(site="06902000", parameterCd = c("00060", "99133"), 
                            startDate = "2018-01-01",
                            endDate = "2019-01-01") %>%
                            renameNWISColumns() %>%
                            rename(Nitrate_mgl = 6) 


#--------plotting C-Q plots--------------#
Hermann.2018.plot <- ggplot(Hermann,
                       aes(x = Flow_Inst, y = Nitrate_mgl)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
print(Hermann.2018.plot) #no pattern of concentration over discharge - chemostatic

Desoto.2018.plot <- ggplot(Desoto,
                      aes(x = Flow_Inst, y = Nitrate_mgl)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
print(Desoto.2018.plot) #nitrate concentration barely changes with change in flow, chemostatic
#conc not dependent on discharge fluxes (not coming from overland flow?)

Clarinda.2018.plot <- ggplot(Clarinda,
                        aes(x = Flow_Inst, y = Nitrate_mgl)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
print(Clarinda.2018.plot) #no discernable patter, huge variation in N conc within 
#small changes of discharge (influenced by season?)

Randolph.2018.plot <- ggplot(Randolph,
                        aes(x = Flow_Inst, y = Nitrate_mgl)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
print(Randolph.2018.plot) #chemostatic no discernable pattern whatsoever





#------------Dygraph to determine flushing or diluting behavior---------#

dygraph(
  cbind(
    Flow = xts(Hermann.2018$Flow_Inst, order.by = Hermann.2018$dateTime), 
    Nitrate = xts(Hermann.2018$Nitrate_mgl, order.by = Hermann.2018$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
#October 6-24 show decrease in N concentration during flood event

dygraph(
  cbind(
    Flow = xts(Clarinda.2018$Flow_Inst, order.by = Clarinda.2018$dateTime), 
    Nitrate = xts(Clarinda.2018$Nitrate_mgl, order.by = Clarinda.2018$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
#Oct flood also showed that as discharge inc, N dec at first

dygraph(
  cbind(
    Flow = xts(Desoto.2018$Flow_Inst, order.by = Desoto.2018$dateTime), 
    Nitrate = xts(Desoto.2018$Nitrate_mgl, order.by = Desoto.2018$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
#Oct missing N data for part of flood

dygraph(
  cbind(
    Flow = xts(Randolph.2018$Flow_Inst, order.by = Randolph.2018$dateTime), 
    Nitrate = xts(Randolph.2018$Nitrate_mgl, order.by = Randolph.2018$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
#Oct same as Clarinda and Hermann

dygraph(
  cbind(
    Flow = xts(Shawnee.2018$Flow_Inst, order.by = Shawnee.2018$dateTime), 
    Nitrate = xts(Shawnee.2018$Nitrate_mgl, order.by = Shawnee.2018$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
#Oct same as Hermann, Clarinda, Randolph

dygraph(
  cbind(
    Flow = xts(Sumner.2018$Flow_Inst, order.by = Sumner.2018$dateTime), 
    Nitrate = xts(Sumner.2018$Nitrate_mgl, order.by = Sumner.2018$dateTime)
  )
) %>% 
  dySeries("Nitrate", axis = "y2") %>%
  dyRangeSelector()
#missing data for october

#Hermann storm
Hermann.2018.Storm <- Hermann.2018 %>%
  filter(dateTime > "2018-10-06" & dateTime < "2018-10-22") 
ggplot(Hermann.2018.Storm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() #not helpful

#Clarinda storm
Clarinda.2018.Storm <- Clarinda.2018 %>%
  filter(dateTime > "2018-10-08" & dateTime < "2018-10-13") 
ggplot(Clarinda.2018.Storm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() 
#counter clockwise, diluting storm

#Randdolph storm
Randolph.2018.Storm <- Randolph.2018 %>%
  filter(dateTime > "2018-10-07" & dateTime < "2018-10-13") 
ggplot(Randolph.2018.Storm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() 
#counter clockwise, diluting storm

#Shawnee storm
Shawnee.2018.Storm <- Shawnee.2018 %>%
  filter(dateTime > "2018-10-09" & dateTime < "2018-10-10") 
ggplot(Shawnee.2018.Storm, aes(x = Flow_Inst, y = Nitrate_mgl, color = dateTime)) +
  geom_point() 
#counter clockwise, diluting storm