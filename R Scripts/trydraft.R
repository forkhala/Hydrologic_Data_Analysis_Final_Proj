## Session Set Up
getwd()

library(tidyverse)
library(dataRetrieval)
library(trend)
library(forecast)
library(tseries)
library(lubridate)

theme_set(theme_classic())

###########################################################################
#################### 1 Platte River near Overton, Nebr. ###################
###########################################################################

PlatteRiverSummary <- whatNWISdata(siteNumbers = "06768000")

########################### Discharge Analysis ############################

PlatteRiverDischarge <- readNWISdv(siteNumbers = "06768000",
                                   parameterCd = "00060", # discharge (ft3/s)
                                   startDate = "",
                                   endDate = "")

PlatteRiverDischarge <- mutate(PlatteRiverDischarge,year(Date))
names(PlatteRiverDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
PlatteRiverDischarge <- PlatteRiverDischarge %>% 
  select("Discharge","year") %>%
  summarise(
    Discharge = sum(Discharge)
  )

mk.test(PlatteRiverDischarge$Discharge)
ggplot(PlatteRiverDischarge, aes(x = year, y = Discharge)) + 
  geom_line()

PlatteRiverNitrogen <- readNWISqw(siteNumbers = "06768000",
                                  parameterCd = "00600", # nitrogen (mg/L)
                                  startDate = "",
                                  endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
PlatteRiverN <-
  ggplot(PlatteRiverNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(PlatteRiverN)

# Remove outliers
PlatteRiverNitrogen <- PlatteRiverNitrogen %>%
  filter(result_va < 6 & sample_dt < "1990-01-01") %>%
  arrange(sample_dt)


PlatteRiverNitrogen <- mutate(PlatteRiverNitrogen,year(sample_dt))
PlatteRiverNitrogen <- PlatteRiverNitrogen %>% 
  select("year(sample_dt)","result_va")
names(PlatteRiverNitrogen)[1] <-c("year") 
PlatteRiverNitrogen <- PlatteRiverNitrogen %>%
  group_by(year) %>%
  summarise(
    N = sum(result_va)
  )

mk.test(PlatteRiverNitrogen$N)
ggplot(PlatteRiverNitrogen, aes(x = year, y = N)) + 
  geom_line()
