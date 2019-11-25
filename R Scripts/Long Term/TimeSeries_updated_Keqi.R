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

###########################################################################
################### 2 Platte River at Louisville, Nebr. ###################
###########################################################################

PRLSummary <- whatNWISdata(siteNumbers = "06805500")

########################### Discharge Analysis ############################

PRLDischarge <- readNWISdv(siteNumbers = "06805500",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
PRLDischarge <- mutate(PRLDischarge,year(Date))
names(PRLDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
PRLDischargemk <- PRLDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge)
  )

mk.test(PRLDischargemk$Discharge)
# ggplot(PRLDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

PRLDischarge.Monthly <- PRLDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
PRLMonthly_ts <- ts(PRLDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(PRLMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
PRLfit <- arima(PRLMonthly_ts, c(0, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
PRLprediction <- predict(PRLfit, n.ahead = 24)
PRLprediction$pred

########################### Nitrogen Analysis ############################

PRLNitrogen <- readNWISqw(siteNumbers = "06805500",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
PRLN <-
  ggplot(PRLNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(PRLN)

# Remove outliers
PRLNitrogen <- PRLNitrogen %>%
  filter(result_va < 25 & sample_dt < "2004-01-01") %>%
  arrange(sample_dt)

# Re-plot data
PRLN <-
  ggplot(PRLNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(PRLN)

# Generate monthly values from November 1974 to September 2003
PRLNitrogenlp <- as.data.frame(approx(PRLNitrogen, n = 348, method = "linear"))
PRLNitrogenlp$x <- as.Date(PRLNitrogenlp$x, origin = "1970-01-01")
names(PRLNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
PRLNinterpolated <-
  ggplot(PRLNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = PRLNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(PRLNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
PRLNtimeseries <- ts(PRLNitrogenlp$N, frequency = 12,
                     start = c(1974, 11, 06), end = c(2003, 9, 19))
# Run SMK test
PRLNtrend <- smk.test(PRLNtimeseries)

# Inspect results
PRLNtrend
summary(PRLNtrend)

########################### Phosphorus Analysis ############################

PRLPhosphorus <- readNWISqw(siteNumbers = "06805500",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
PRLP <-
  ggplot(PRLPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(PRLP)

# Remove outliers
PRLPhosphorus <- PRLPhosphorus %>%
  filter(result_va < 4 & sample_dt < "2020-01-01") %>%
  arrange(sample_dt)

# Re-plot data
PRLP <-
  ggplot(PRLPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(PRLP)

# Generate monthly values from November 1974 to August 2019
PRLPhosphoruslp <- as.data.frame(approx(PRLPhosphorus, n = 539, method = "linear"))
PRLPhosphoruslp$x <- as.Date(PRLPhosphoruslp$x, origin = "1970-01-01")
names(PRLPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
PRLPinterpolated <-
  ggplot(PRLPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = PRLPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(PRLPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
PRLPtimeseries <- ts(PRLPhosphoruslp$P, frequency = 12,
                     start = c(1974, 11, 6), end = c(2019, 8, 28))
# Run SMK test
PRLPtrend <- smk.test(PRLPtimeseries)

# Inspect results
PRLPtrend
summary(PRLPtrend)

###########################################################################
################### 3 Dismal River near Thedford, Nebr. ###################
###########################################################################

DRTSummary <- whatNWISdata(siteNumbers = "06775900")

########################### Discharge Analysis ############################

DRTDischarge <- readNWISdv(siteNumbers = "06775900",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
DRTDischarge <- mutate(DRTDischarge,year(Date))

DRTDischarge <- DRTDischarge %>%
  filter(Date > "1973-01-01") %>%
  arrange(Date)

names(DRTDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
DRTDischargemk <- DRTDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge)
  )

mk.test(DRTDischargemk$Discharge)
# ggplot(DRTDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

DRTDischarge.Monthly <- DRTDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
DRTMonthly_ts <- ts(DRTDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(DRTMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
DRTfit <- arima(DRTMonthly_ts, c(1, 1, 3),seasonal = list(order = c(0, 0, 1), period = 12))

# make a prediction into the future
DRTprediction <- predict(DRTfit, n.ahead = 24)
DRTprediction$pred

########################### Nitrogen Analysis ############################

DRTNitrogen <- readNWISqw(siteNumbers = "06775900",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
DRTN <-
  ggplot(DRTNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(DRTN)

# Remove outliers
DRTNitrogen <- DRTNitrogen %>%
  filter(result_va < 2 & sample_dt < "2004-01-01" & sample_dt > "1980-01-01") %>%
  arrange(sample_dt)

# Re-plot data
DRTN <-
  ggplot(DRTNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(DRTN)

# Generate monthly values from November 1980 to September 2003
DRTNitrogenlp <- as.data.frame(approx(DRTNitrogen, n = 276, method = "linear"))
DRTNitrogenlp$x <- as.Date(DRTNitrogenlp$x, origin = "1970-01-01")
names(DRTNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
DRTNinterpolated <-
  ggplot(DRTNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = DRTNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(DRTNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
DRTNtimeseries <- ts(DRTNitrogenlp$N, frequency = 12,
                     start = c(1980, 11, 25), end = c(2003, 9, 8))
# Run SMK test
DRTNtrend <- smk.test(DRTNtimeseries)

# Inspect results
DRTNtrend
summary(DRTNtrend)

########################### Phosphorus Analysis ############################

DRTPhosphorus <- readNWISqw(siteNumbers = "06775900",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
DRTP <-
  ggplot(DRTPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(DRTP)

# Remove outliers
DRTPhosphorus <- DRTPhosphorus %>%
  filter(sample_dt < "2005-01-01" & sample_dt > "1975-01-01") %>%
  arrange(sample_dt)

# Re-plot data
DRTP <-
  ggplot(DRTPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(DRTP)

# Generate monthly values from March 1975 to September 2004
DRTPhosphoruslp <- as.data.frame(approx(DRTPhosphorus, n = 356, method = "linear"))
DRTPhosphoruslp$x <- as.Date(DRTPhosphoruslp$x, origin = "1970-01-01")
names(DRTPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
DRTPinterpolated <-
  ggplot(DRTPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = DRTPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(DRTPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
DRTPtimeseries <- ts(DRTPhosphoruslp$P, frequency = 12,
                     start = c(1975, 3, 26), end = c(2004, 9, 14))
# Run SMK test
DRTPtrend <- smk.test(DRTPtimeseries)

# Inspect results
DRTPtrend
summary(DRTPtrend)

###########################################################################
################### 4 Beaver Creek at Genoa, Nebr. ########################
###########################################################################

BCGSummary <- whatNWISdata(siteNumbers = "06794000")

########################### Discharge Analysis ############################

BCGDischarge <- readNWISdv(siteNumbers = "06794000",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
BCGDischarge <- mutate(BCGDischarge,year(Date))
names(BCGDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
BCGDischargemk <- BCGDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge)
  )

mk.test(BCGDischargemk$Discharge)
# ggplot(BCGDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

BCGDischarge.Monthly <- BCGDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
BCGMonthly_ts <- ts(BCGDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(BCGMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
BCGfit <- arima(BCGMonthly_ts, c(1, 0, 1),seasonal = list(order = c(2, 0, 0), period = 12))

# make a prediction into the future
BCGprediction <- predict(BCGfit, n.ahead = 24)
BCGprediction$pred

########################### Nitrogen Analysis ############################

BCGNitrogen <- readNWISqw(siteNumbers = "06794000",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
BCGN <-
  ggplot(BCGNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(BCGN)

# Remove outliers
BCGNitrogen <- BCGNitrogen %>%
  filter(result_va < 10) %>%
  arrange(sample_dt)

# Re-plot data
BCGN <-
  ggplot(BCGNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(BCGN)

# Generate monthly values from October 1978 to September 1989
BCGNitrogenlp <- as.data.frame(approx(BCGNitrogen, n = 133, method = "linear"))
BCGNitrogenlp$x <- as.Date(BCGNitrogenlp$x, origin = "1970-01-01")
names(BCGNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
BCGNinterpolated <-
  ggplot(BCGNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = BCGNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(BCGNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
BCGNtimeseries <- ts(BCGNitrogenlp$N, frequency = 12,
                     start = c(1978, 10, 25), end = c(1989, 9, 8))
# Run SMK test
BCGNtrend <- smk.test(BCGNtimeseries)

# Inspect results
BCGNtrend
summary(BCGNtrend)

########################### Phosphorus Analysis ############################

BCGPhosphorus <- readNWISqw(siteNumbers = "06794000",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
BCGP <-
  ggplot(BCGPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(BCGP)

# Remove outliers
BCGPhosphorus <- BCGPhosphorus %>%
  filter(result_va < 3 & sample_dt > "1975-01-01") %>%
  arrange(sample_dt)

# Re-plot data
BCGP <-
  ggplot(BCGPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(BCGP)

# Generate monthly values from October 1978 to September 1989
BCGPhosphoruslp <- as.data.frame(approx(BCGPhosphorus, n = 133, method = "linear"))
BCGPhosphoruslp$x <- as.Date(BCGPhosphoruslp$x, origin = "1970-01-01")
names(BCGPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
BCGPinterpolated <-
  ggplot(BCGPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = BCGPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(BCGPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
BCGPtimeseries <- ts(BCGPhosphoruslp$P, frequency = 12,
                     start = c(1978, 10, 25), end = c(1989, 9, 8))
# Run SMK test
BCGPtrend <- smk.test(BCGPtimeseries)

# Inspect results
BCGPtrend
summary(BCGPtrend)

###########################################################################
################### 5 Maple Creek near Nickerson, Nebr. ###################
###########################################################################

MCNSummary <- whatNWISdata(siteNumbers = "06800000")

########################### Discharge Analysis ############################

MCNDischarge <- readNWISdv(siteNumbers = "06800000",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
MCNDischarge <- mutate(MCNDischarge,year(Date))
names(MCNDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
MCNDischargemk <- MCNDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(MCNDischargemk$Discharge)
# ggplot(MCNDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

MCNDischarge.Monthly <- MCNDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MCNMonthly_ts <- ts(MCNDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(MCNMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MCNfit <- arima(MCNMonthly_ts, c(0, 1, 4),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
MCNprediction <- predict(MCNfit, n.ahead = 24)
MCNprediction$pred

########################### Nitrogen Analysis ############################

MCNNitrogen <- readNWISqw(siteNumbers = "06800000",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MCNN <-
  ggplot(MCNNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MCNN)

# Remove outliers
MCNNitrogen <- MCNNitrogen %>%
  filter(result_va < 60 & sample_dt > "2012-01-01") %>%
  arrange(sample_dt)

# Re-plot data
MCNN <-
  ggplot(MCNNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(MCNN)

# Generate monthly values from October 2012 to October 2019
MCNNitrogenlp <- as.data.frame(approx(MCNNitrogen, n = 86, method = "linear"))
MCNNitrogenlp$x <- as.Date(MCNNitrogenlp$x, origin = "1970-01-01")
names(MCNNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
MCNNinterpolated <-
  ggplot(MCNNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MCNNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(MCNNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MCNNtimeseries <- ts(MCNNitrogenlp$N, frequency = 12,
                     start = c(2012, 10, 17), end = c(2019, 10, 17))
# Run SMK test
MCNNtrend <- smk.test(MCNNtimeseries)

# Inspect results
MCNNtrend
summary(MCNNtrend)

########################### Phosphorus Analysis ############################

MCNPhosphorus <- readNWISqw(siteNumbers = "06800000",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MCNP <-
  ggplot(MCNPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MCNP)

# Remove outliers
MCNPhosphorus <- MCNPhosphorus %>%
  filter(result_va < 10 & sample_dt > "2011-01-01") %>%
  arrange(sample_dt)

# Re-plot data
MCNP <-
  ggplot(MCNPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(MCNP)

# Generate monthly values from October 2011 to October 2019
MCNPhosphoruslp <- as.data.frame(approx(MCNPhosphorus, n = 98, method = "linear"))
MCNPhosphoruslp$x <- as.Date(MCNPhosphoruslp$x, origin = "1970-01-01")
names(MCNPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
MCNPinterpolated <-
  ggplot(MCNPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MCNPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(MCNPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MCNPtimeseries <- ts(MCNPhosphoruslp$P, frequency = 12,
                     start = c(2011, 10, 17), end = c(2019, 10, 17))
# Run SMK test
MCNPtrend <- smk.test(MCNPtimeseries)

# Inspect results
MCNPtrend
summary(MCNPtrend)

###########################################################################
################### 6 Elkhorn River at Waterloo, Nebr. ###################
###########################################################################

ERWSummary <- whatNWISdata(siteNumbers = "06800500")

########################### Discharge Analysis ############################

ERWDischarge <- readNWISdv(siteNumbers = "06800500",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
ERWDischarge <- mutate(ERWDischarge,year(Date))

ERWDischarge <- ERWDischarge %>%
  filter(Date > "1928-01-01") %>%
  arrange(Date)

names(ERWDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
ERWDischargemk <- ERWDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(ERWDischargemk$Discharge)
# ggplot(ERWDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

ERWDischarge.Monthly <- ERWDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
ERWMonthly_ts <- ts(ERWDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(ERWMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
ERWfit <- arima(ERWMonthly_ts, c(0, 1, 5),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
ERWprediction <- predict(ERWfit, n.ahead = 24)
ERWprediction$pred

########################### Nitrogen Analysis ############################

ERWNitrogen <- readNWISqw(siteNumbers = "06800500",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
ERWN <-
  ggplot(ERWNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(ERWN)

# Remove outliers
ERWNitrogen <- ERWNitrogen %>%
  filter(result_va < 25 & sample_dt > "2012-01-01") %>%
  arrange(sample_dt)

# Re-plot data
ERWN <-
  ggplot(ERWNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(ERWN)

# Generate monthly values from October 2012 to October 2019
ERWNitrogenlp <- as.data.frame(approx(ERWNitrogen, n = 86, method = "linear"))
ERWNitrogenlp$x <- as.Date(ERWNitrogenlp$x, origin = "1970-01-01")
names(ERWNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
ERWNinterpolated <-
  ggplot(ERWNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = ERWNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(ERWNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
ERWNtimeseries <- ts(ERWNitrogenlp$N, frequency = 12,
                     start = c(2012, 10, 22), end = c(2019, 10, 16))
# Run SMK test
ERWNtrend <- smk.test(ERWNtimeseries)

# Inspect results
ERWNtrend
summary(ERWNtrend)

########################### Phosphorus Analysis ############################

ERWPhosphorus <- readNWISqw(siteNumbers = "06800500",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
ERWP <-
  ggplot(ERWPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(ERWP)

# Remove outliers
ERWPhosphorus <- ERWPhosphorus %>%
  filter(result_va < 9 & sample_dt > "2012-01-01") %>%
  arrange(sample_dt)

# Re-plot data
ERWP <-
  ggplot(ERWPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(ERWP)

# Generate monthly values from October 2012 to October 2019
ERWPhosphoruslp <- as.data.frame(approx(ERWPhosphorus, n = 86, method = "linear"))
ERWPhosphoruslp$x <- as.Date(ERWPhosphoruslp$x, origin = "1970-01-01")
names(ERWPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
ERWPinterpolated <-
  ggplot(ERWPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = ERWPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(ERWPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
ERWPtimeseries <- ts(ERWPhosphoruslp$P, frequency = 12,
                     start = c(2012, 10, 22), end = c(2019, 10, 16))
# Run SMK test
ERWPtrend <- smk.test(ERWPtimeseries)

# Inspect results
ERWPtrend
summary(ERWPtrend)

###########################################################################
######################### 7 Missouri River at Omaha, NE ###################
###########################################################################

MROSummary <- whatNWISdata(siteNumbers = "06610000")

########################### Discharge Analysis ############################

MRODischarge <- readNWISdv(siteNumbers = "06610000",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
MRODischarge <- mutate(MRODischarge,year(Date))

names(MRODischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
MRODischargemk <- MRODischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(MRODischargemk$Discharge)
# ggplot(MRODischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

MRODischarge.Monthly <- MRODischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MROMonthly_ts <- ts(MRODischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(MROMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MROfit <- arima(MROMonthly_ts, c(2, 0, 1),seasonal = list(order = c(1, 1, 1), period = 12))

# make a prediction into the future
MROprediction <- predict(MROfit, n.ahead = 24)
MROprediction$pred

########################### Nitrogen Analysis ############################

MRONitrogen <- readNWISqw(siteNumbers = "06610000",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MRON <-
  ggplot(MRONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MRON)

# Remove outliers
MRONitrogen <- MRONitrogen %>%
  filter(result_va < 20 & sample_dt > "1998-01-01") %>%
  arrange(sample_dt)

# Re-plot data
MRON <-
  ggplot(MRONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(MRON)

# Generate monthly values from October 1998 to October 2019
MRONitrogenlp <- as.data.frame(approx(MRONitrogen, n = 254, method = "linear"))
MRONitrogenlp$x <- as.Date(MRONitrogenlp$x, origin = "1970-01-01")
names(MRONitrogenlp) <- c("Date", "N")

# Inspect interpolated values
MRONinterpolated <-
  ggplot(MRONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MRONitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(MRONinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MRONtimeseries <- ts(MRONitrogenlp$N, frequency = 12,
                     start = c(1998, 10, 20), end = c(2019, 10, 08))
# Run SMK test
MRONtrend <- smk.test(MRONtimeseries)

# Inspect results
MRONtrend
summary(MRONtrend)

########################### Phosphorus Analysis ############################

MROPhosphorus <- readNWISqw(siteNumbers = "06610000",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MROP <-
  ggplot(MROPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MROP)

# Remove outliers
MROPhosphorus <- MROPhosphorus %>%
  filter(result_va < 4 & sample_dt > "1998-01-01") %>%
  arrange(sample_dt)

# Re-plot data
MROP <-
  ggplot(MROPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(MROP)

# Generate monthly values from October 1998 to October 2019
MROPhosphoruslp <- as.data.frame(approx(MROPhosphorus, n = 254, method = "linear"))
MROPhosphoruslp$x <- as.Date(MROPhosphoruslp$x, origin = "1970-01-01")
names(MROPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
MROPinterpolated <-
  ggplot(MROPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MROPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(MROPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MROPtimeseries <- ts(MROPhosphoruslp$P, frequency = 12,
                     start = c(1998, 10, 20), end = c(2019, 10, 08))
# Run SMK test
MROPtrend <- smk.test(MROPtimeseries)

# Inspect results
MROPtrend
summary(MROPtrend)

###########################################################################
######################### 8 Boyer River at Logan, IA ######################
###########################################################################

BRLSummary <- whatNWISdata(siteNumbers = "06609500")

########################### Discharge Analysis ############################

BRLDischarge <- readNWISdv(siteNumbers = "06609500",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
BRLDischarge <- mutate(BRLDischarge,year(Date))

BRLDischarge <- BRLDischarge %>%
  filter(Date > "1937-01-01") %>%
  arrange(Date)

names(BRLDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
BRLDischargemk <- BRLDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(BRLDischargemk$Discharge)
# ggplot(BRLDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

BRLDischarge.Monthly <- BRLDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
BRLMonthly_ts <- ts(BRLDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(BRLMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
BRLfit <- arima(BRLMonthly_ts, c(1, 1, 2),seasonal = list(order = c(0, 0, 1), period = 12))

# make a prediction into the future
BRLprediction <- predict(BRLfit, n.ahead = 24)
BRLprediction$pred

########################### Nitrogen Analysis ############################

BRLNitrogen <- readNWISqw(siteNumbers = "06609500",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
BRLN <-
  ggplot(BRLNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(BRLN)

# Remove outliers
BRLNitrogen <- BRLNitrogen %>%
  filter(result_va < 30) %>%
  arrange(sample_dt)

# Re-plot data
BRLN <-
  ggplot(BRLNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(BRLN)

# Generate monthly values from March 2004 to September 2014
BRLNitrogenlp <- as.data.frame(approx(BRLNitrogen, n = 128, method = "linear"))
BRLNitrogenlp$x <- as.Date(BRLNitrogenlp$x, origin = "1970-01-01")
names(BRLNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
BRLNinterpolated <-
  ggplot(BRLNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = BRLNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(BRLNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
BRLNtimeseries <- ts(BRLNitrogenlp$N, frequency = 12,
                     start = c(2004, 3, 12), end = c(2014, 9, 15))
# Run SMK test
BRLNtrend <- smk.test(BRLNtimeseries)

# Inspect results
BRLNtrend
summary(BRLNtrend)

########################### Phosphorus Analysis ############################

BRLPhosphorus <- readNWISqw(siteNumbers = "06609500",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
BRLP <-
  ggplot(BRLPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(BRLP)

# Remove outliers
BRLPhosphorus <- BRLPhosphorus %>%
  filter(result_va < 5) %>%
  arrange(sample_dt)

# Re-plot data
BRLP <-
  ggplot(BRLPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(BRLP)

# Generate monthly values from March 2004 to September 2014
BRLPhosphoruslp <- as.data.frame(approx(BRLPhosphorus, n = 128, method = "linear"))
BRLPhosphoruslp$x <- as.Date(BRLPhosphoruslp$x, origin = "1970-01-01")
names(BRLPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
BRLPinterpolated <-
  ggplot(BRLPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = BRLPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(BRLPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
BRLPtimeseries <- ts(BRLPhosphoruslp$P, frequency = 12,
                     start = c(2004, 3, 12), end = c(2014, 9, 15))
# Run SMK test
BRLPtrend <- smk.test(BRLPtimeseries)

# Inspect results
BRLPtrend
summary(BRLPtrend)

###########################################################################
################# 9 Nishnabotna River above Hamburg, IA ###################
###########################################################################

NRHSummary <- whatNWISdata(siteNumbers = "06810000")

########################### Discharge Analysis ############################

NRHDischarge <- readNWISdv(siteNumbers = "06810000",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
NRHDischarge <- mutate(NRHDischarge,year(Date))

NRHDischarge <- NRHDischarge %>%
  filter(Date > "1928-01-01") %>%
  arrange(Date)

names(NRHDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
NRHDischargemk <- NRHDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(NRHDischargemk$Discharge)
# ggplot(NRHDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

NRHDischarge.Monthly <- NRHDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
NRHMonthly_ts <- ts(NRHDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(NRHMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
NRHfit <- arima(NRHMonthly_ts, c(1, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
NRHprediction <- predict(NRHfit, n.ahead = 24)
NRHprediction$pred

########################### Nitrogen Analysis ############################

NRHNitrogen <- readNWISqw(siteNumbers = "06810000",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
NRHN <-
  ggplot(NRHNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(NRHN)

# Remove outliers
NRHNitrogen <- NRHNitrogen %>%
  filter(result_va < 20 & sample_dt > "2004-01-01") %>%
  arrange(sample_dt)

# Re-plot data
NRHN <-
  ggplot(NRHNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(NRHN)

# Generate monthly values from March 2004 to September 2014
NRHNitrogenlp <- as.data.frame(approx(NRHNitrogen, n = 128, method = "linear"))
NRHNitrogenlp$x <- as.Date(NRHNitrogenlp$x, origin = "1970-01-01")
names(NRHNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
NRHNinterpolated <-
  ggplot(NRHNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = NRHNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(NRHNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
NRHNtimeseries <- ts(NRHNitrogenlp$N, frequency = 12,
                     start = c(2004, 3, 11), end = c(2014, 9, 16))
# Run SMK test
NRHNtrend <- smk.test(NRHNtimeseries)

# Inspect results
NRHNtrend
summary(NRHNtrend)

########################### Phosphorus Analysis ############################

NRHPhosphorus <- readNWISqw(siteNumbers = "06810000",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
NRHP <-
  ggplot(NRHPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(NRHP)

# Remove outliers
NRHPhosphorus <- NRHPhosphorus %>%
  filter(result_va < 5 & sample_dt > "2004-01-01") %>%
  arrange(sample_dt)

# Re-plot data
NRHP <-
  ggplot(NRHPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(NRHP)

# Generate monthly values from March 2004 to September 2014
NRHPhosphoruslp <- as.data.frame(approx(NRHPhosphorus, n = 128, method = "linear"))
NRHPhosphoruslp$x <- as.Date(NRHPhosphoruslp$x, origin = "1970-01-01")
names(NRHPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
NRHPinterpolated <-
  ggplot(NRHPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = NRHPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(NRHPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
NRHPtimeseries <- ts(NRHPhosphoruslp$P, frequency = 12,
                     start = c(2004, 3, 11), end = c(2014, 9, 16))
# Run SMK test
NRHPtrend <- smk.test(NRHPtimeseries)

# Inspect results
NRHPtrend
summary(NRHPtrend)

###########################################################################
################# 10 Missouri River at St. Joseph, MO #####################
###########################################################################

MRSJSummary <- whatNWISdata(siteNumbers = "06818000")

########################### Discharge Analysis ############################

MRSJDischarge <- readNWISdv(siteNumbers = "06818000",
                            parameterCd = "00060", # discharge (ft3/s)
                            startDate = "",
                            endDate = "")
MRSJDischarge <- mutate(MRSJDischarge,year(Date))

names(MRSJDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
MRSJDischargemk <- MRSJDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(MRSJDischargemk$Discharge)
# ggplot(MRSJDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

MRSJDischarge.Monthly <- MRSJDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MRSJMonthly_ts <- ts(MRSJDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(MRSJMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MRSJfit <- arima(MRSJMonthly_ts, c(2, 0, 1),seasonal = list(order = c(2, 1, 0), period = 12))

# make a prediction into the future
MRSJprediction <- predict(MRSJfit, n.ahead = 24)
MRSJprediction$pred

########################### Nitrogen Analysis ############################

MRSJNitrogen <- readNWISqw(siteNumbers = "06818000",
                           parameterCd = "00600", # nitrogen (mg/L)
                           startDate = "",
                           endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MRSJN <-
  ggplot(MRSJNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MRSJN)

# Remove outliers
MRSJNitrogen <- MRSJNitrogen %>%
  filter(result_va < 15) %>%
  arrange(sample_dt)

# Re-plot data
MRSJN <-
  ggplot(MRSJNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(MRSJN)

# Generate monthly values from July 1973 to September 2019
MRSJNitrogenlp <- as.data.frame(approx(MRSJNitrogen, n = 496, method = "linear"))
MRSJNitrogenlp$x <- as.Date(MRSJNitrogenlp$x, origin = "1970-01-01")
names(MRSJNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
MRSJNinterpolated <-
  ggplot(MRSJNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MRSJNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(MRSJNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MRSJNtimeseries <- ts(MRSJNitrogenlp$N, frequency = 12,
                      start = c(1973, 7, 17), end = c(2019, 9, 24))
# Run SMK test
MRSJNtrend <- smk.test(MRSJNtimeseries)

# Inspect results
MRSJNtrend
summary(MRSJNtrend)

########################### Phosphorus Analysis ############################

MRSJPhosphorus <- readNWISqw(siteNumbers = "06818000",
                             parameterCd = "00665", # phosphorus (mg/L)
                             startDate = "",
                             endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MRSJP <-
  ggplot(MRSJPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MRSJP)

# Remove outliers
MRSJPhosphorus <- MRSJPhosphorus %>%
  filter(result_va < 3 & sample_dt > "1998-01-01") %>%
  arrange(sample_dt)

# Re-plot data
MRSJP <-
  ggplot(MRSJPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(MRSJP)

# Generate monthly values from November 1998 to September 2019
MRSJPhosphoruslp <- as.data.frame(approx(MRSJPhosphorus, n = 192, method = "linear"))
MRSJPhosphoruslp$x <- as.Date(MRSJPhosphoruslp$x, origin = "1970-01-01")
names(MRSJPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
MRSJPinterpolated <-
  ggplot(MRSJPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MRSJPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(MRSJPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MRSJPtimeseries <- ts(MRSJPhosphoruslp$P, frequency = 12,
                      start = c(1998, 11, 17), end = c(2019, 9, 24))
# Run SMK test
MRSJPtrend <- smk.test(MRSJPtimeseries)

# Inspect results
MRSJPtrend
summary(MRSJPtrend)

###########################################################################
############### 11 Republican River near Orleans, Nebr. ###################
###########################################################################

RROSummary <- whatNWISdata(siteNumbers = "06844500")

########################### Discharge Analysis ############################

RRODischarge <- readNWISdv(siteNumbers = "06844500",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
RRODischarge <- mutate(RRODischarge,year(Date))

names(RRODischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
RRODischargemk <- RRODischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(RRODischargemk$Discharge)
# ggplot(RRODischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

RRODischarge.Monthly <- RRODischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
RROMonthly_ts <- ts(RRODischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(RROMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
RROfit <- arima(RROMonthly_ts, c(1, 1, 1),seasonal = list(order = c(2, 0, 0), period = 12))

# make a prediction into the future
RROprediction <- predict(RROfit, n.ahead = 24)
RROprediction$pred

########################### Nitrogen Analysis ############################

RRONitrogen <- readNWISqw(siteNumbers = "06844500",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
RRON <-
  ggplot(RRONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(RRON)

# Remove outliers
RRONitrogen <- RRONitrogen %>%
  filter(result_va < 7.5) %>%
  arrange(sample_dt)

# Re-plot data
RRON <-
  ggplot(RRONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(RRON)

# Generate monthly values from March 1973 to August 1989
RRONitrogenlp <- as.data.frame(approx(RRONitrogen, n = 189, method = "linear"))
RRONitrogenlp$x <- as.Date(RRONitrogenlp$x, origin = "1970-01-01")
names(RRONitrogenlp) <- c("Date", "N")

# Inspect interpolated values
RRONinterpolated <-
  ggplot(RRONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = RRONitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(RRONinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
RRONtimeseries <- ts(RRONitrogenlp$N, frequency = 12,
                     start = c(1973, 3, 21), end = c(1989, 8, 28))
# Run SMK test
RRONtrend <- smk.test(RRONtimeseries)

# Inspect results
RRONtrend
summary(RRONtrend)

########################### Phosphorus Analysis ############################

RROPhosphorus <- readNWISqw(siteNumbers = "06844500",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
RROP <-
  ggplot(RROPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(RROP)

# Remove outliers
RROPhosphorus <- RROPhosphorus %>%
  filter(result_va < 2 & sample_dt > "1973-01-01") %>%
  arrange(sample_dt)

# Re-plot data
RROP <-
  ggplot(RROPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(RROP)

# Generate monthly values from March 1973 to August 1989
RROPhosphoruslp <- as.data.frame(approx(RROPhosphorus, n = 189, method = "linear"))
RROPhosphoruslp$x <- as.Date(RROPhosphoruslp$x, origin = "1970-01-01")
names(RROPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
RROPinterpolated <-
  ggplot(RROPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = RROPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(RROPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
RROPtimeseries <- ts(RROPhosphoruslp$P, frequency = 12,
                     start = c(1973, 3, 21), end = c(1989, 8, 28))
# Run SMK test
RROPtrend <- smk.test(RROPtimeseries)

# Inspect results
RROPtrend
summary(RROPtrend)

###########################################################################
############### 12 REPUBLICAN R AT CLAY CENTER, KS ########################
###########################################################################

RRCCSummary <- whatNWISdata(siteNumbers = "06856600")

########################### Discharge Analysis ############################

RRCCDischarge <- readNWISdv(siteNumbers = "06856600",
                            parameterCd = "00060", # discharge (ft3/s)
                            startDate = "",
                            endDate = "")
RRCCDischarge <- mutate(RRCCDischarge,year(Date))

names(RRCCDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
RRCCDischargemk <- RRCCDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(RRCCDischargemk$Discharge)
# ggplot(RRCCDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

RRCCDischarge.Monthly <- RRCCDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
RRCCMonthly_ts <- ts(RRCCDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(RRCCMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
RRCCfit <- arima(RRCCMonthly_ts, c(0, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
RRCCprediction <- predict(RRCCfit, n.ahead = 24)
RRCCprediction$pred

########################### Nitrogen Analysis ############################

RRCCNitrogen <- readNWISqw(siteNumbers = "06856600",
                           parameterCd = "00600", # nitrogen (mg/L)
                           startDate = "",
                           endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
RRCCN <-
  ggplot(RRCCNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(RRCCN)

# Remove outliers
RRCCNitrogen <- RRCCNitrogen %>%
  filter(result_va < 8 & sample_dt < "1994-01-01") %>%
  arrange(sample_dt)

# Re-plot data
RRCCN <-
  ggplot(RRCCNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(RRCCN)

# Generate monthly values from February 1973 to September 1993
RRCCNitrogenlp <- as.data.frame(approx(RRCCNitrogen, n = 249, method = "linear"))
RRCCNitrogenlp$x <- as.Date(RRCCNitrogenlp$x, origin = "1970-01-01")
names(RRCCNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
RRCCNinterpolated <-
  ggplot(RRCCNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = RRCCNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(RRCCNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
RRCCNtimeseries <- ts(RRCCNitrogenlp$N, frequency = 12,
                      start = c(1973, 2, 22), end = c(1993, 9, 1))
# Run SMK test
RRCCNtrend <- smk.test(RRCCNtimeseries)

# Inspect results
RRCCNtrend
summary(RRCCNtrend)

########################### Phosphorus Analysis ############################

RRCCPhosphorus <- readNWISqw(siteNumbers = "06856600",
                             parameterCd = "00665", # phosphorus (mg/L)
                             startDate = "",
                             endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
RRCCP <-
  ggplot(RRCCPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(RRCCP)

# Remove outliers
RRCCPhosphorus <- RRCCPhosphorus %>%
  filter(result_va < 2 & sample_dt < "1994-01-01") %>%
  arrange(sample_dt)

# Re-plot data
RRCCP <-
  ggplot(RRCCPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(RRCCP)

# Generate monthly values from January 1973 to September 1993
RRCCPhosphoruslp <- as.data.frame(approx(RRCCPhosphorus, n = 250, method = "linear"))
RRCCPhosphoruslp$x <- as.Date(RRCCPhosphoruslp$x, origin = "1970-01-01")
names(RRCCPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
RRCCPinterpolated <-
  ggplot(RRCCPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = RRCCPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(RRCCPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
RRCCPtimeseries <- ts(RRCCPhosphoruslp$P, frequency = 12,
                      start = c(1973, 1, 16), end = c(1993, 9, 1))
# Run SMK test
RRCCPtrend <- smk.test(RRCCPtimeseries)

# Inspect results
RRCCPtrend
summary(RRCCPtrend)

###########################################################################
############### 13 SMOKY HILL R AT ENTERPRISE, KS ########################
###########################################################################

SHESummary <- whatNWISdata(siteNumbers = "06877600")

########################### Discharge Analysis ############################

SHEDischarge <- readNWISdv(siteNumbers = "06877600",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
SHEDischarge <- mutate(SHEDischarge,year(Date))

names(SHEDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
SHEDischargemk <- SHEDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(SHEDischargemk$Discharge)
# ggplot(SHEDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

SHEDischarge.Monthly <- SHEDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
SHEMonthly_ts <- ts(SHEDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(SHEMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
SHEfit <- arima(SHEMonthly_ts, c(1, 0, 1),seasonal = list(order = c(1, 0, 0), period = 12))

# make a prediction into the future
SHEprediction <- predict(SHEfit, n.ahead = 24)
SHEprediction$pred

########################### Nitrogen Analysis ############################

SHENitrogen <- readNWISqw(siteNumbers = "06877600",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
SHEN <-
  ggplot(SHENitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(SHEN)

# Remove outliers
SHENitrogen <- SHENitrogen %>%
  filter(result_va < 6) %>%
  arrange(sample_dt)

# Re-plot data
SHEN <-
  ggplot(SHENitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(SHEN)

# Generate monthly values from March 1974 to July 1995
SHENitrogenlp <- as.data.frame(approx(SHENitrogen, n = 270, method = "linear"))
SHENitrogenlp$x <- as.Date(SHENitrogenlp$x, origin = "1970-01-01")
names(SHENitrogenlp) <- c("Date", "N")

# Inspect interpolated values
SHENinterpolated <-
  ggplot(SHENitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = SHENitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(SHENinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
SHENtimeseries <- ts(SHENitrogenlp$N, frequency = 12,
                     start = c(1974, 3, 12), end = c(1995, 7, 18))
# Run SMK test
SHENtrend <- smk.test(SHENtimeseries)

# Inspect results
SHENtrend
summary(SHENtrend)

########################### Phosphorus Analysis ############################

SHEPhosphorus <- readNWISqw(siteNumbers = "06877600",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
SHEP <-
  ggplot(SHEPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(SHEP)

# Remove outliers
SHEPhosphorus <- SHEPhosphorus %>%
  filter(result_va < 1.5) %>%
  arrange(sample_dt)

# Re-plot data
SHEP <-
  ggplot(SHEPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(SHEP)

# Generate monthly values from October 1971 to July 1995
SHEPhosphoruslp <- as.data.frame(approx(SHEPhosphorus, n = 287, method = "linear"))
SHEPhosphoruslp$x <- as.Date(SHEPhosphoruslp$x, origin = "1970-01-01")
names(SHEPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
SHEPinterpolated <-
  ggplot(SHEPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = SHEPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(SHEPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
SHEPtimeseries <- ts(SHEPhosphoruslp$P, frequency = 12,
                     start = c(1971, 10, 6), end = c(1995, 7, 18))
# Run SMK test
SHEPtrend <- smk.test(SHEPtimeseries)

# Inspect results
SHEPtrend
summary(SHEPtrend)

###########################################################################
################### 14 SF SOLOMON R AT OSBORNE, KS ########################
###########################################################################

SSOSummary <- whatNWISdata(siteNumbers = "06874000")

########################### Discharge Analysis ############################

SSODischarge <- readNWISdv(siteNumbers = "06874000",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
SSODischarge <- mutate(SSODischarge,year(Date))

names(SSODischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
SSODischargemk <- SSODischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(SSODischargemk$Discharge)
# ggplot(SSODischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

SSODischarge.Monthly <- SSODischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
SSOMonthly_ts <- ts(SSODischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(SSOMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
SSOfit <- arima(SSOMonthly_ts, c(1, 0, 0))

# make a prediction into the future
SSOprediction <- predict(SSOfit, n.ahead = 24)
SSOprediction$pred

########################### Nitrogen Analysis ############################

SSONitrogen <- readNWISqw(siteNumbers = "06874000",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
SSON <-
  ggplot(SSONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(SSON)

# Remove outliers
SSONitrogen <- SSONitrogen %>%
  filter(result_va < 8) %>%
  arrange(sample_dt)

# Re-plot data
SSON <-
  ggplot(SSONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(SSON)

# Generate monthly values from November 1977 to August 1994
SSONitrogenlp <- as.data.frame(approx(SSONitrogen, n = 203, method = "linear"))
SSONitrogenlp$x <- as.Date(SSONitrogenlp$x, origin = "1970-01-01")
names(SSONitrogenlp) <- c("Date", "N")

# Inspect interpolated values
SSONinterpolated <-
  ggplot(SSONitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = SSONitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(SSONinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
SSONtimeseries <- ts(SSONitrogenlp$N, frequency = 12,
                     start = c(1977, 11, 2), end = c(1994, 8, 24))
# Run SMK test
SSONtrend <- smk.test(SSONtimeseries)

# Inspect results
SSONtrend
summary(SSONtrend)

########################### Phosphorus Analysis ############################

SSOPhosphorus <- readNWISqw(siteNumbers = "06874000",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
SSOP <-
  ggplot(SSOPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(SSOP)

# Remove outliers
SSOPhosphorus <- SSOPhosphorus %>%
  filter(result_va < 2 & sample_dt > "1977-01-01") %>%
  arrange(sample_dt)

# Re-plot data
SSOP <-
  ggplot(SSOPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(SSOP)

# Generate monthly values from November 1977 to August 1994
SSOPhosphoruslp <- as.data.frame(approx(SSOPhosphorus, n = 203, method = "linear"))
SSOPhosphoruslp$x <- as.Date(SSOPhosphoruslp$x, origin = "1970-01-01")
names(SSOPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
SSOPinterpolated <-
  ggplot(SSOPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = SSOPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(SSOPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
SSOPtimeseries <- ts(SSOPhosphoruslp$P, frequency = 12,
                     start = c(1977, 11, 2), end = c(1994, 8, 24))
# Run SMK test
SSOPtrend <- smk.test(SSOPtimeseries)

# Inspect results
SSOPtrend
summary(SSOPtrend)

###########################################################################
################### 15 KANSAS R AT WAMEGO, KS #############################
###########################################################################

KWSummary <- whatNWISdata(siteNumbers = "06887500")

########################### Discharge Analysis ############################

KWDischarge <- readNWISdv(siteNumbers = "06887500",
                          parameterCd = "00060", # discharge (ft3/s)
                          startDate = "",
                          endDate = "")
KWDischarge <- mutate(KWDischarge,year(Date))

names(KWDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
KWDischargemk <- KWDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(KWDischargemk$Discharge)
# ggplot(KWDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

KWDischarge.Monthly <- KWDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
KWMonthly_ts <- ts(KWDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(KWMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
KWfit <- arima(KWMonthly_ts, c(1, 0, 0),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
KWprediction <- predict(KWfit, n.ahead = 24)
KWprediction$pred

########################### Nitrogen Analysis ############################

KWNitrogen <- readNWISqw(siteNumbers = "06887500",
                         parameterCd = "00600", # nitrogen (mg/L)
                         startDate = "",
                         endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
KWN <-
  ggplot(KWNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(KWN)

# Remove outliers
KWNitrogen <- KWNitrogen %>%
  filter(result_va < 7.5 & sample_dt > "2012-01-01") %>%
  arrange(sample_dt)

# Re-plot data
KWN <-
  ggplot(KWNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(KWN)

# Generate monthly values from July 2012 to September 2019
KWNitrogenlp <- as.data.frame(approx(KWNitrogen, n = 89, method = "linear"))
KWNitrogenlp$x <- as.Date(KWNitrogenlp$x, origin = "1970-01-01")
names(KWNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
KWNinterpolated <-
  ggplot(KWNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = KWNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(KWNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
KWNtimeseries <- ts(KWNitrogenlp$N, frequency = 12,
                    start = c(2012, 7, 19), end = c(2019, 9, 23))
# Run SMK test
KWNtrend <- smk.test(KWNtimeseries)

# Inspect results
KWNtrend
summary(KWNtrend)

########################### Phosphorus Analysis ############################

KWPhosphorus <- readNWISqw(siteNumbers = "06887500",
                           parameterCd = "00665", # phosphorus (mg/L)
                           startDate = "",
                           endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
KWP <-
  ggplot(KWPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(KWP)

# Remove outliers
KWPhosphorus <- KWPhosphorus %>%
  filter(result_va < 2 & sample_dt > "2012-01-01") %>%
  arrange(sample_dt)

# Re-plot data
KWP <-
  ggplot(KWPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(KWP)

# Generate monthly values from July 2012 to September 2019
KWPhosphoruslp <- as.data.frame(approx(KWPhosphorus, n = 89, method = "linear"))
KWPhosphoruslp$x <- as.Date(KWPhosphoruslp$x, origin = "1970-01-01")
names(KWPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
KWPinterpolated <-
  ggplot(KWPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = KWPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(KWPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
KWPtimeseries <- ts(KWPhosphoruslp$P, frequency = 12,
                    start = c(2012, 7, 19), end = c(2019, 9, 23))
# Run SMK test
KWPtrend <- smk.test(KWPtimeseries)

# Inspect results
KWPtrend
summary(KWPtrend)

###########################################################################
################### 16 KANSAS R AT DESOTO, KS #############################
###########################################################################

KDSummary <- whatNWISdata(siteNumbers = "06892350")

########################### Discharge Analysis ############################

KDDischarge <- readNWISdv(siteNumbers = "06892350",
                          parameterCd = "00060", # discharge (ft3/s)
                          startDate = "",
                          endDate = "")
KDDischarge <- mutate(KDDischarge,year(Date))

names(KDDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
KDDischargemk <- KDDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(KDDischargemk$Discharge)
# ggplot(KDDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

KDDischarge.Monthly <- KDDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
KDMonthly_ts <- ts(KDDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(KDMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
KDfit <- arima(KDMonthly_ts, c(1, 0, 0),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
KDprediction <- predict(KDfit, n.ahead = 24)
KDprediction$pred

########################### Nitrogen Analysis ############################

KDNitrogen <- readNWISqw(siteNumbers = "06892350",
                         parameterCd = "00600", # nitrogen (mg/L)
                         startDate = "",
                         endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
KDN <-
  ggplot(KDNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(KDN)

# Remove outliers
KDNitrogen <- KDNitrogen %>%
  filter(result_va < 10 & sample_dt > "2012-01-01") %>%
  arrange(sample_dt)

# Re-plot data
KDN <-
  ggplot(KDNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(KDN)

# Generate monthly values from July 2012 to September 2019
KDNitrogenlp <- as.data.frame(approx(KDNitrogen, n = 89, method = "linear"))
KDNitrogenlp$x <- as.Date(KDNitrogenlp$x, origin = "1970-01-01")
names(KDNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
KDNinterpolated <-
  ggplot(KDNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = KDNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(KDNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
KDNtimeseries <- ts(KDNitrogenlp$N, frequency = 12,
                    start = c(2012, 7, 19), end = c(2019, 9, 24))
# Run SMK test
KDNtrend <- smk.test(KDNtimeseries)

# Inspect results
KDNtrend
summary(KDNtrend)

########################### Phosphorus Analysis ############################

KDPhosphorus <- readNWISqw(siteNumbers = "06892350",
                           parameterCd = "00665", # phosphorus (mg/L)
                           startDate = "",
                           endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
KDP <-
  ggplot(KDPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(KDP)

# Remove outliers
KDPhosphorus <- KDPhosphorus %>%
  filter(result_va < 5 & sample_dt > "2012-01-01") %>%
  arrange(sample_dt)

# Re-plot data
KDP <-
  ggplot(KDPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(KDP)

# Generate monthly values from July 2012 to September 2019
KDPhosphoruslp <- as.data.frame(approx(KDPhosphorus, n = 89, method = "linear"))
KDPhosphoruslp$x <- as.Date(KDPhosphoruslp$x, origin = "1970-01-01")
names(KDPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
KDPinterpolated <-
  ggplot(KDPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = KDPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(KDPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
KDPtimeseries <- ts(KDPhosphoruslp$P, frequency = 12,
                    start = c(2012, 7, 19), end = c(2019, 9, 24))
# Run SMK test
KDPtrend <- smk.test(KDPtimeseries)

# Inspect results
KDPtrend
summary(KDPtrend)

###########################################################################
################### 17 Grand River near Sumner, MO ########################
###########################################################################

GRSSummary <- whatNWISdata(siteNumbers = "06902000")

########################### Discharge Analysis ############################

GRSDischarge <- readNWISdv(siteNumbers = "06902000",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
GRSDischarge <- mutate(GRSDischarge,year(Date))

names(GRSDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
GRSDischargemk <- GRSDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(GRSDischargemk$Discharge)
# ggplot(GRSDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

GRSDischarge.Monthly <- GRSDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
GRSMonthly_ts <- ts(GRSDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(GRSMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
GRSfit <- arima(GRSMonthly_ts, c(0, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
GRSprediction <- predict(GRSfit, n.ahead = 24)
GRSprediction$pred

########################### Nitrogen Analysis ############################

GRSNitrogen <- readNWISqw(siteNumbers = "06902000",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
GRSN <-
  ggplot(GRSNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(GRSN)

# Remove outliers
GRSNitrogen <- GRSNitrogen %>%
  filter(result_va < 20) %>%
  arrange(sample_dt)

# Re-plot data
GRSN <-
  ggplot(GRSNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(GRSN)

# Generate monthly values from July 1973 to September 2019
GRSNitrogenlp <- as.data.frame(approx(GRSNitrogen, n = 557, method = "linear"))
GRSNitrogenlp$x <- as.Date(GRSNitrogenlp$x, origin = "1970-01-01")
names(GRSNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
GRSNinterpolated <-
  ggplot(GRSNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = GRSNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(GRSNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
GRSNtimeseries <- ts(GRSNitrogenlp$N, frequency = 12,
                     start = c(1973, 7, 17), end = c(2019, 9, 9))
# Run SMK test
GRSNtrend <- smk.test(GRSNtimeseries)

# Inspect results
GRSNtrend
summary(GRSNtrend)

########################### Phosphorus Analysis ############################

GRSPhosphorus <- readNWISqw(siteNumbers = "06902000",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
GRSP <-
  ggplot(GRSPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(GRSP)

# Remove outliers
GRSPhosphorus <- GRSPhosphorus %>%
  filter(result_va < 3) %>%
  arrange(sample_dt)

# Re-plot data
GRSP <-
  ggplot(GRSPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(GRSP)

# Generate monthly values from August 1969 to September 2019
GRSPhosphoruslp <- as.data.frame(approx(GRSPhosphorus, n = 567, method = "linear"))
GRSPhosphoruslp$x <- as.Date(GRSPhosphoruslp$x, origin = "1970-01-01")
names(GRSPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
GRSPinterpolated <-
  ggplot(GRSPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = GRSPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(GRSPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
GRSPtimeseries <- ts(GRSPhosphoruslp$P, frequency = 12,
                     start = c(1969, 8, 20), end = c(2019, 9, 9))
# Run SMK test
GRSPtrend <- smk.test(GRSPtimeseries)

# Inspect results
GRSPtrend
summary(GRSPtrend)

###########################################################################
############## 18 Chariton River near Prairie Hill, MO ####################
###########################################################################

CRPSummary <- whatNWISdata(siteNumbers = "06905500")

########################### Discharge Analysis ############################

CRPDischarge <- readNWISdv(siteNumbers = "06905500",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
CRPDischarge <- mutate(CRPDischarge,year(Date))

names(CRPDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
CRPDischargemk <- CRPDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(CRPDischargemk$Discharge)
# ggplot(CRPDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

CRPDischarge.Monthly <- CRPDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
CRPMonthly_ts <- ts(CRPDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(CRPMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
CRPfit <- arima(CRPMonthly_ts, c(0, 1, 2),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
CRPprediction <- predict(CRPfit, n.ahead = 24)
CRPprediction$pred

########################### Nitrogen Analysis ############################

CRPNitrogen <- readNWISqw(siteNumbers = "06905500",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
CRPN <-
  ggplot(CRPNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(CRPN)

# Remove outliers
CRPNitrogen <- CRPNitrogen %>%
  filter(result_va < 10 & sample_dt > "1992-01-01") %>%
  arrange(sample_dt)

# Re-plot data
CRPN <-
  ggplot(CRPNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(CRPN)

# Generate monthly values from November 1992 to September 2019
CRPNitrogenlp <- as.data.frame(approx(CRPNitrogen, n = 324, method = "linear"))
CRPNitrogenlp$x <- as.Date(CRPNitrogenlp$x, origin = "1970-01-01")
names(CRPNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
CRPNinterpolated <-
  ggplot(CRPNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = CRPNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(CRPNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
CRPNtimeseries <- ts(CRPNitrogenlp$N, frequency = 12,
                     start = c(1992, 11, 12), end = c(2019, 9, 10))
# Run SMK test
CRPNtrend <- smk.test(CRPNtimeseries)

# Inspect results
CRPNtrend
summary(CRPNtrend)

########################### Phosphorus Analysis ############################

CRPPhosphorus <- readNWISqw(siteNumbers = "06905500",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
CRPP <-
  ggplot(CRPPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(CRPP)

# Remove outliers
CRPPhosphorus <- CRPPhosphorus %>%
  filter(result_va < 2 & sample_dt > "1992-01-01") %>%
  arrange(sample_dt)

# Re-plot data
CRPP <-
  ggplot(CRPPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(CRPP)

# Generate monthly values from November 1992 to September 2019
CRPPhosphoruslp <- as.data.frame(approx(CRPPhosphorus, n = 324, method = "linear"))
CRPPhosphoruslp$x <- as.Date(CRPPhosphoruslp$x, origin = "1970-01-01")
names(CRPPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
CRPPinterpolated <-
  ggplot(CRPPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = CRPPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(CRPPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
CRPPtimeseries <- ts(CRPPhosphoruslp$P, frequency = 12,
                     start = c(1992, 11, 12), end = c(2019, 9, 10))
# Run SMK test
CRPPtrend <- smk.test(CRPPtimeseries)

# Inspect results
CRPPtrend
summary(CRPPtrend)

###########################################################################
############## 19 Pomme de Terre River near Polk, MO ######################
###########################################################################

PTRPSummary <- whatNWISdata(siteNumbers = "06921070")

########################### Discharge Analysis ############################

PTRPDischarge <- readNWISdv(siteNumbers = "06921070",
                            parameterCd = "00060", # discharge (ft3/s)
                            startDate = "",
                            endDate = "")
PTRPDischarge <- mutate(PTRPDischarge,year(Date))

names(PTRPDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
PTRPDischargemk <- PTRPDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(PTRPDischargemk$Discharge)
# ggplot(PTRPDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

PTRPDischarge.Monthly <- PTRPDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
PTRPMonthly_ts <- ts(PTRPDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(PTRPMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
PTRPfit <- arima(PTRPMonthly_ts, c(1, 0, 0),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
PTRPprediction <- predict(PTRPfit, n.ahead = 24)
PTRPprediction$pred

########################### Nitrogen Analysis ############################

PTRPNitrogen <- readNWISqw(siteNumbers = "06921070",
                           parameterCd = "00600", # nitrogen (mg/L)
                           startDate = "",
                           endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
PTRPN <-
  ggplot(PTRPNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(PTRPN)

# Remove outliers
PTRPNitrogen <- PTRPNitrogen %>%
  filter(result_va < 4) %>%
  arrange(sample_dt)

# Re-plot data
PTRPN <-
  ggplot(PTRPNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(PTRPN)

# Generate monthly values from November 1992 to September 2019
PTRPNitrogenlp <- as.data.frame(approx(PTRPNitrogen, n = 324, method = "linear"))
PTRPNitrogenlp$x <- as.Date(PTRPNitrogenlp$x, origin = "1970-01-01")
names(PTRPNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
PTRPNinterpolated <-
  ggplot(PTRPNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = PTRPNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(PTRPNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
PTRPNtimeseries <- ts(PTRPNitrogenlp$N, frequency = 12,
                      start = c(1992, 11, 17), end = c(2019, 9, 10))
# Run SMK test
PTRPNtrend <- smk.test(PTRPNtimeseries)

# Inspect results
PTRPNtrend
summary(PTRPNtrend)

########################### Phosphorus Analysis ############################

PTRPPhosphorus <- readNWISqw(siteNumbers = "06921070",
                             parameterCd = "00665", # phosphorus (mg/L)
                             startDate = "",
                             endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
PTRPP <-
  ggplot(PTRPPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(PTRPP)

# Remove outliers
PTRPPhosphorus <- PTRPPhosphorus %>%
  filter(result_va < 0.6 & sample_dt > "1992-01-01") %>%
  arrange(sample_dt)

# Re-plot data
PTRPP <-
  ggplot(PTRPPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(PTRPP)

# Generate monthly values from November 1992 to September 2019
PTRPPhosphoruslp <- as.data.frame(approx(PTRPPhosphorus, n = 324, method = "linear"))
PTRPPhosphoruslp$x <- as.Date(PTRPPhosphoruslp$x, origin = "1970-01-01")
names(PTRPPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
PTRPPinterpolated <-
  ggplot(PTRPPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = PTRPPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(PTRPPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
PTRPPtimeseries <- ts(PTRPPhosphoruslp$P, frequency = 12,
                      start = c(1992, 11, 17), end = c(2019, 9, 10))
# Run SMK test
PTRPPtrend <- smk.test(PTRPPtimeseries)

# Inspect results
PTRPPtrend
summary(PTRPPtrend)

###########################################################################
############## 20 Osage River below St. Thomas, MO ########################
###########################################################################

ORSTSummary <- whatNWISdata(siteNumbers = "06926510")

########################### Discharge Analysis ############################

ORSTDischarge <- readNWISdv(siteNumbers = "06926510",
                            parameterCd = "00060", # discharge (ft3/s)
                            startDate = "",
                            endDate = "")
ORSTDischarge <- mutate(ORSTDischarge,year(Date))

ORSTDischarge <- ORSTDischarge %>%
  filter(Date > "2001-01-01") %>%
  arrange(Date)

names(ORSTDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
ORSTDischargemk <- ORSTDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(ORSTDischargemk$Discharge)
# ggplot(ORSTDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

ORSTDischarge.Monthly <- ORSTDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
ORSTMonthly_ts <- ts(ORSTDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(ORSTMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
ORSTfit <- arima(ORSTMonthly_ts, c(1, 0, 0),seasonal = list(order = c(1, 0, 0), period = 12))

# make a prediction into the future
ORSTprediction <- predict(ORSTfit, n.ahead = 24)
ORSTprediction$pred

########################### Nitrogen Analysis ############################

ORSTNitrogen <- readNWISqw(siteNumbers = "06926510",
                           parameterCd = "00600", # nitrogen (mg/L)
                           startDate = "",
                           endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
ORSTN <-
  ggplot(ORSTNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(ORSTN)

# Remove outliers
ORSTNitrogen <- ORSTNitrogen %>%
  filter(result_va < 5) %>%
  arrange(sample_dt)

# Re-plot data
ORSTN <-
  ggplot(ORSTNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(ORSTN)

# Generate monthly values from October 1974 to October 2019
ORSTNitrogenlp <- as.data.frame(approx(ORSTNitrogen, n = 542, method = "linear"))
ORSTNitrogenlp$x <- as.Date(ORSTNitrogenlp$x, origin = "1970-01-01")
names(ORSTNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
ORSTNinterpolated <-
  ggplot(ORSTNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = ORSTNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(ORSTNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
ORSTNtimeseries <- ts(ORSTNitrogenlp$N, frequency = 12,
                      start = c(1974, 10, 18), end = c(2019, 10, 2))
# Run SMK test
ORSTNtrend <- smk.test(ORSTNtimeseries)

# Inspect results
ORSTNtrend
summary(ORSTNtrend)

########################### Phosphorus Analysis ############################

ORSTPhosphorus <- readNWISqw(siteNumbers = "06926510",
                             parameterCd = "00665", # phosphorus (mg/L)
                             startDate = "",
                             endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
ORSTP <-
  ggplot(ORSTPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(ORSTP)

# Remove outliers
ORSTPhosphorus <- ORSTPhosphorus %>%
  filter(result_va < 0.5) %>%
  arrange(sample_dt)

# Re-plot data
ORSTP <-
  ggplot(ORSTPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(ORSTP)

# Generate monthly values from October 1974 to October 2019
ORSTPhosphoruslp <- as.data.frame(approx(ORSTPhosphorus, n = 542, method = "linear"))
ORSTPhosphoruslp$x <- as.Date(ORSTPhosphoruslp$x, origin = "1970-01-01")
names(ORSTPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
ORSTPinterpolated <-
  ggplot(ORSTPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = ORSTPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(ORSTPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
ORSTPtimeseries <- ts(ORSTPhosphoruslp$P, frequency = 12,
                      start = c(1974, 10, 18), end = c(2019, 10, 2))
# Run SMK test
ORSTPtrend <- smk.test(ORSTPtimeseries)

# Inspect results
ORSTPtrend
summary(ORSTPtrend)

###########################################################################
############## 21 Little Blue River near Lake City, MO ####################
###########################################################################

LBRLCSummary <- whatNWISdata(siteNumbers = "06894000")

########################### Discharge Analysis ############################

LBRLCDischarge <- readNWISdv(siteNumbers = "06894000",
                             parameterCd = "00060", # discharge (ft3/s)
                             startDate = "",
                             endDate = "")
LBRLCDischarge <- mutate(LBRLCDischarge,year(Date))

names(LBRLCDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
LBRLCDischargemk <- LBRLCDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(LBRLCDischargemk$Discharge)
# ggplot(LBRLCDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

LBRLCDischarge.Monthly <- LBRLCDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
LBRLCMonthly_ts <- ts(LBRLCDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(LBRLCMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
LBRLCfit <- arima(LBRLCMonthly_ts, c(0, 1, 2),seasonal = list(order = c(0, 0, 1), period = 12))

# make a prediction into the future
LBRLCprediction <- predict(LBRLCfit, n.ahead = 24)
LBRLCprediction$pred

########################### Nitrogen Analysis ############################

LBRLCNitrogen <- readNWISqw(siteNumbers = "06894000",
                            parameterCd = "00600", # nitrogen (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
LBRLCN <-
  ggplot(LBRLCNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(LBRLCN)

# Remove outliers
LBRLCNitrogen <- LBRLCNitrogen %>%
  filter(result_va < 10) %>%
  arrange(sample_dt)

# Re-plot data
LBRLCN <-
  ggplot(LBRLCNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(LBRLCN)

# Generate monthly values from June 2005 to October 2019
LBRLCNitrogenlp <- as.data.frame(approx(LBRLCNitrogen, n = 174, method = "linear"))
LBRLCNitrogenlp$x <- as.Date(LBRLCNitrogenlp$x, origin = "1970-01-01")
names(LBRLCNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
LBRLCNinterpolated <-
  ggplot(LBRLCNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = LBRLCNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(LBRLCNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
LBRLCNtimeseries <- ts(LBRLCNitrogenlp$N, frequency = 12,
                       start = c(2005, 6, 13), end = c(2019, 10, 16))
# Run SMK test
LBRLCNtrend <- smk.test(LBRLCNtimeseries)

# Inspect results
LBRLCNtrend
summary(LBRLCNtrend)

########################### Phosphorus Analysis ############################

LBRLCPhosphorus <- readNWISqw(siteNumbers = "06894000",
                              parameterCd = "00665", # phosphorus (mg/L)
                              startDate = "",
                              endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
LBRLCP <-
  ggplot(LBRLCPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(LBRLCP)

# Remove outliers
LBRLCPhosphorus <- LBRLCPhosphorus %>%
  filter(result_va < 4) %>%
  arrange(sample_dt)

# Re-plot data
LBRLCP <-
  ggplot(LBRLCPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(LBRLCP)

# Generate monthly values from June 2005 to October 2019
LBRLCPhosphoruslp <- as.data.frame(approx(LBRLCPhosphorus, n = 174, method = "linear"))
LBRLCPhosphoruslp$x <- as.Date(LBRLCPhosphoruslp$x, origin = "1970-01-01")
names(LBRLCPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
LBRLCPinterpolated <-
  ggplot(LBRLCPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = LBRLCPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(LBRLCPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
LBRLCPtimeseries <- ts(LBRLCPhosphoruslp$P, frequency = 12,
                       start = c(2005, 6, 13), end = c(2019, 10, 16))
# Run SMK test
LBRLCPtrend <- smk.test(LBRLCPtimeseries)

# Inspect results
LBRLCPtrend
summary(LBRLCPtrend)

###########################################################################
############## 22 Missouri River at Hermann, MO ###########################
###########################################################################

MRHSummary <- whatNWISdata(siteNumbers = "06934500")

########################### Discharge Analysis ############################

MRHDischarge <- readNWISdv(siteNumbers = "06934500",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
MRHDischarge <- mutate(MRHDischarge,year(Date))

names(MRHDischarge)[4:6] <-c("Discharge", "Approval.Code","year") 
MRHDischargemk <- MRHDischarge %>% 
  select("Discharge","year") %>%
  group_by(year) %>%
  summarise(
    Discharge = sum(Discharge, na.rm = TRUE)
  )

mk.test(MRHDischargemk$Discharge)
# ggplot(MRHDischargemk, aes(x = year, y = Discharge)) + 
#   geom_line()

MRHDischarge.Monthly <- MRHDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MRHMonthly_ts <- ts(MRHDischarge.Monthly[[3]], frequency = 12)

# run the arima function and search for best fit
auto.arima(MRHMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MRHfit <- arima(MRHMonthly_ts, c(5, 1, 0),seasonal = list(order = c(2, 0, 0), period = 12))

# make a prediction into the future
MRHprediction <- predict(MRHfit, n.ahead = 24)
MRHprediction$pred

########################### Nitrogen Analysis ############################

MRHNitrogen <- readNWISqw(siteNumbers = "06934500",
                          parameterCd = "00600", # nitrogen (mg/L)
                          startDate = "",
                          endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MRHN <-
  ggplot(MRHNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MRHN)

# Remove outliers
MRHNitrogen <- MRHNitrogen %>%
  filter(result_va < 10) %>%
  arrange(sample_dt)

# Re-plot data
MRHN <-
  ggplot(MRHNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("nitrogen (mg/L)")) 
print(MRHN)

# Generate monthly values from July 1973 to August 2019
MRHNitrogenlp <- as.data.frame(approx(MRHNitrogen, n = 556, method = "linear"))
MRHNitrogenlp$x <- as.Date(MRHNitrogenlp$x, origin = "1970-01-01")
names(MRHNitrogenlp) <- c("Date", "N")

# Inspect interpolated values
MRHNinterpolated <-
  ggplot(MRHNitrogen, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MRHNitrogenlp, aes(x = Date, y = N), color = "#c13d75ff")
print(MRHNinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MRHNtimeseries <- ts(MRHNitrogenlp$N, frequency = 12,
                     start = c(1973, 7, 23), end = c(2019, 8, 6))
# Run SMK test
MRHNtrend <- smk.test(MRHNtimeseries)

# Inspect results
MRHNtrend
summary(MRHNtrend)

########################### Phosphorus Analysis ############################

MRHPhosphorus <- readNWISqw(siteNumbers = "06934500",
                            parameterCd = "00665", # phosphorus (mg/L)
                            startDate = "",
                            endDate = "") %>%
  select(sample_dt, result_va)

# What do these data look like?
MRHP <-
  ggplot(MRHPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point()
print(MRHP)

# Remove outliers
MRHPhosphorus <- MRHPhosphorus %>%
  filter(result_va < 3) %>%
  arrange(sample_dt)

# Re-plot data
MRHP <-
  ggplot(MRHPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = expression("phosphorus (mg/L)")) 
print(MRHP)

# Generate monthly values from July 1969 to August 2019
MRHPhosphoruslp <- as.data.frame(approx(MRHPhosphorus, n = 604, method = "linear"))
MRHPhosphoruslp$x <- as.Date(MRHPhosphoruslp$x, origin = "1970-01-01")
names(MRHPhosphoruslp) <- c("Date", "P")

# Inspect interpolated values
MRHPinterpolated <-
  ggplot(MRHPhosphorus, aes(x = sample_dt, y = result_va)) +
  geom_point() +
  geom_line() +
  geom_point(data = MRHPhosphoruslp, aes(x = Date, y = P), color = "#c13d75ff")
print(MRHPinterpolated)

# Generate time series (smk.test needs ts, not data.frame)
MRHPtimeseries <- ts(MRHPhosphoruslp$P, frequency = 12,
                     start = c(1969, 7, 31), end = c(2019, 8, 6))
# Run SMK test
MRHPtrend <- smk.test(MRHPtimeseries)

# Inspect results
MRHPtrend
summary(MRHPtrend)
