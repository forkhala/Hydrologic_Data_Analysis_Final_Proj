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

########################### Discharge Analysis ############################

PlatteRiverDischarge <- readNWISdv(siteNumbers = "06768000",
                                   parameterCd = "00060", # discharge (ft3/s)
                                   startDate = "",
                                   endDate = "")

PlatteRiverDischarge <- mutate(PlatteRiverDischarge,year(Date))
names(PlatteRiverDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
PlatteRiverDischargemk <- PlatteRiverDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge)
  )

mk.test(PlatteRiverDischargemk$Discharge)
# ggplot(PlatteRiverDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

PlatteRiverDischarge.Monthly <- PlatteRiverDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
PlatteRiverMonthly_ts <- ts(PlatteRiverDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(PlatteRiverMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
PlatteRiverfit <- arima(PlatteRiverMonthly_ts, c(1, 1, 2),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
PlatteRiverprediction <- predict(PlatteRiverfit, n.ahead = 24)
PlatteRiverprediction$pred

########################### Nitrogen Analysis ############################

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

# Re-plot data
PlatteRiverN <-
  ggplot(PlatteRiverNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(PlatteRiverN)

# Generate monthly values from October 1978 to August 1989
PlatteRiverNitrogenlp <- as.data.frame(approx(PlatteRiverNitrogen, n = 132, method = "linear"))
PlatteRiverNitrogenlp$x <- as.Date(PlatteRiverNitrogenlp$x, origin = "1970-01-01")
names(PlatteRiverNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
PlatteRiverNinterpolated <-
  ggplot(PlatteRiverNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = PlatteRiverNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(PlatteRiverNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
PlatteRiverNtimeseries <- ts(PlatteRiverNitrogenlp$N, frequency = 12,
                             start = c(1978, 10, 24), end = c(1989, 8, 30))
# Run SMK test
PlatteRiverNtrend <- smk.test(PlatteRiverNtimeseries)

# Inspect results
PlatteRiverNtrend
summary(PlatteRiverNtrend)

########################### Phosphorus Analysis ############################

PlatteRiverPhosphorus <- readNWISqw(siteNumbers = "06768000",
                                    parameterCd = "00665", # phosphorus (mg/L)
                                    startDate = "",
                                    endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
PlatteRiverP <-
  ggplot(PlatteRiverPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(PlatteRiverP)

# Remove outliers
PlatteRiverPhosphorus <- PlatteRiverPhosphorus %>%
  filter(result_va < 0.4 & sample_dt < "1990-01-01") %>%
  arrange(sample_dt)

# Re-plot data
PlatteRiverP <-
  ggplot(PlatteRiverPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(PlatteRiverP)

# Generate monthly values from October 1978 to August 1989
PlatteRiverPhosphoruslp <- as.data.frame(approx(PlatteRiverPhosphorus, n = 132, method = "linear"))
PlatteRiverPhosphoruslp$x <- as.Date(PlatteRiverPhosphoruslp$x, origin = "1970-01-01")
names(PlatteRiverPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
PlatteRiverPinterpolated <-
  ggplot(PlatteRiverPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = PlatteRiverPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(PlatteRiverPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
PlatteRiverPtimeseries <- ts(PlatteRiverPhosphoruslp$P, frequency = 12,
                             start = c(1978, 10, 24), end = c(1989, 8, 30))
# Run SMK test
PlatteRiverPtrend <- smk.test(PlatteRiverPtimeseries)

# Inspect results
PlatteRiverPtrend
summary(PlatteRiverPtrend)

