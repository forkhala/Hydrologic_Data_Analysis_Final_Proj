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
names(PlatteRiverDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
PlatteRiverPlot <- 
  ggplot(PlatteRiverDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(PlatteRiverPlot)

PlatteRiverRegressionPlot <- 
  ggplot(PlatteRiverDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(PlatteRiverRegressionPlot)

PlatteRiver_ts <- ts(PlatteRiverDischarge[[4]], frequency = 365)
table(diff(PlatteRiverDischarge$Date))

# Generate the decomposition
PlatteRiver_Decomposed <- stl(PlatteRiver_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(PlatteRiver_Decomposed)

# We can extract the components and turn them into data frames
PlatteRiver_Components <- as.data.frame(PlatteRiver_Decomposed$time.series[,1:3])
PlatteRiver_Components <- mutate(PlatteRiver_Components,
                                Observed = PlatteRiverDischarge$Discharge,     
                                Date = PlatteRiverDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(PlatteRiver_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(PlatteRiver_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

PlatteRiverDischarge.Monthly <- PlatteRiverDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
PlatteRiverMonthly_ts <- ts(PlatteRiverDischarge.Monthly[[3]], frequency = 12)
adf.test(PlatteRiverMonthly_ts, alternative = "stationary")
acf(PlatteRiverMonthly_ts)
pacf(PlatteRiverMonthly_ts)

# run the arima function and search for best fit
auto.arima(PlatteRiverMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
PlatteRiverfit <- arima(PlatteRiverMonthly_ts, c(1, 1, 2),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
PlatteRiverprediction <- predict(PlatteRiverfit, n.ahead = 10*12)

# plot future predictions
ts.plot(PlatteRiverMonthly_ts, PlatteRiverprediction$pred, lty = c(1, 3))

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
names(PRLDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
PRLPlot <- 
  ggplot(PRLDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(PRLPlot)

PRLRegressionPlot <- 
  ggplot(PRLDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(PRLRegressionPlot)

PRL_ts <- ts(PRLDischarge[[4]], frequency = 365)
table(diff(PRLDischarge$Date))

# Generate the decomposition
PRL_Decomposed <- stl(PRL_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(PRL_Decomposed)

# We can extract the components and turn them into data frames
PRL_Components <- as.data.frame(PRL_Decomposed$time.series[,1:3])
PRL_Components <- mutate(PRL_Components,
                                 Observed = PRLDischarge$Discharge,     
                                 Date = PRLDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(PRL_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(PRL_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

PRLDischarge.Monthly <- PRLDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
PRLMonthly_ts <- ts(PRLDischarge.Monthly[[3]], frequency = 12)
adf.test(PRLMonthly_ts, alternative = "stationary")
acf(PRLMonthly_ts)
pacf(PRLMonthly_ts)

# run the arima function and search for best fit
auto.arima(PRLMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
PRLfit <- arima(PRLMonthly_ts, c(0, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
PRLprediction <- predict(PRLfit, n.ahead = 10*12)

# plot future predictions
ts.plot(PRLMonthly_ts, PRLprediction$pred, lty = c(1, 3))

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
names(DRTDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
DRTPlot <- 
  ggplot(DRTDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(DRTPlot)

DRTDischarge <- DRTDischarge %>%
  filter(Date > "1973-01-01") %>%
  arrange(Date)

# Replot discharge over time
DRTPlot <- 
  ggplot(DRTDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(DRTPlot)

DRTRegressionPlot <- 
  ggplot(DRTDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(DRTRegressionPlot)

DRT_ts <- ts(DRTDischarge[[4]], frequency = 365)
table(diff(DRTDischarge$Date))

# Generate the decomposition
DRT_Decomposed <- stl(DRT_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(DRT_Decomposed)

# We can extract the components and turn them into data frames
DRT_Components <- as.data.frame(DRT_Decomposed$time.series[,1:3])
DRT_Components <- mutate(DRT_Components,
                         Observed = DRTDischarge$Discharge,     
                         Date = DRTDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(DRT_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(DRT_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

DRTDischarge.Monthly <- DRTDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
DRTMonthly_ts <- ts(DRTDischarge.Monthly[[3]], frequency = 12)
adf.test(DRTMonthly_ts, alternative = "stationary")
acf(DRTMonthly_ts)
pacf(DRTMonthly_ts)

# run the arima function and search for best fit
auto.arima(DRTMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
DRTfit <- arima(DRTMonthly_ts, c(2, 1, 2),seasonal = list(order = c(0, 0, 1), period = 12))

# make a prediction into the future
DRTprediction <- predict(DRTfit, n.ahead = 10*12)

# plot future predictions
ts.plot(DRTMonthly_ts, DRTprediction$pred, lty = c(1, 3))

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
names(BCGDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
BCGPlot <- 
  ggplot(BCGDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(BCGPlot)

BCGRegressionPlot <- 
  ggplot(BCGDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(BCGRegressionPlot)

BCG_ts <- ts(BCGDischarge[[4]], frequency = 365)
table(diff(BCGDischarge$Date))

# Generate the decomposition
BCG_Decomposed <- stl(BCG_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(BCG_Decomposed)

# We can extract the components and turn them into data frames
BCG_Components <- as.data.frame(BCG_Decomposed$time.series[,1:3])
BCG_Components <- mutate(BCG_Components,
                         Observed = BCGDischarge$Discharge,     
                         Date = BCGDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(BCG_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(BCG_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

BCGDischarge.Monthly <- BCGDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
BCGMonthly_ts <- ts(BCGDischarge.Monthly[[3]], frequency = 12)
adf.test(BCGMonthly_ts, alternative = "stationary")
acf(BCGMonthly_ts)
pacf(BCGMonthly_ts)

# run the arima function and search for best fit
auto.arima(BCGMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
BCGfit <- arima(BCGMonthly_ts, c(1, 0, 1),seasonal = list(order = c(2, 0, 0), period = 12))

# make a prediction into the future
BCGprediction <- predict(BCGfit, n.ahead = 10*12)

# plot future predictions
ts.plot(BCGMonthly_ts, BCGprediction$pred, lty = c(1, 3))

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
names(MCNDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
MCNPlot <- 
  ggplot(MCNDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MCNPlot)

MCNRegressionPlot <- 
  ggplot(MCNDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MCNRegressionPlot)

MCNDischarge <- na.omit(MCNDischarge)
MCN_ts <- ts(MCNDischarge[[4]], frequency = 365)
table(diff(MCNDischarge$Date))

# Generate the decomposition
MCN_Decomposed <- stl(MCN_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(MCN_Decomposed)

# We can extract the components and turn them into data frames
MCN_Components <- as.data.frame(MCN_Decomposed$time.series[,1:3])
MCN_Components <- mutate(MCN_Components,
                         Observed = MCNDischarge$Discharge,     
                         Date = MCNDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(MCN_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(MCN_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

MCNDischarge.Monthly <- MCNDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MCNMonthly_ts <- ts(MCNDischarge.Monthly[[3]], frequency = 12)
adf.test(MCNMonthly_ts, alternative = "stationary")
acf(MCNMonthly_ts)
pacf(MCNMonthly_ts)

# run the arima function and search for best fit
auto.arima(MCNMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MCNfit <- arima(MCNMonthly_ts, c(2, 1, 4),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
MCNprediction <- predict(MCNfit, n.ahead = 10*12)

# plot future predictions
ts.plot(MCNMonthly_ts, MCNprediction$pred, lty = c(1, 3))

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
names(ERWDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
ERWPlot <- 
  ggplot(ERWDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(ERWPlot)

ERWDischarge <- ERWDischarge %>%
  filter(Date > "1928-01-01") %>%
  arrange(Date)

# Replot discharge over time
ERWPlot <- 
  ggplot(ERWDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(ERWPlot)

ERWRegressionPlot <- 
  ggplot(ERWDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(ERWRegressionPlot)

ERWDischarge <- na.omit(ERWDischarge)
ERW_ts <- ts(ERWDischarge[[4]], frequency = 365)
table(diff(ERWDischarge$Date))

# Generate the decomposition
ERW_Decomposed <- stl(ERW_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(ERW_Decomposed)

# We can extract the components and turn them into data frames
ERW_Components <- as.data.frame(ERW_Decomposed$time.series[,1:3])
ERW_Components <- mutate(ERW_Components,
                         Observed = ERWDischarge$Discharge,     
                         Date = ERWDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(ERW_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(ERW_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

ERWDischarge.Monthly <- ERWDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
ERWMonthly_ts <- ts(ERWDischarge.Monthly[[3]], frequency = 12)
adf.test(ERWMonthly_ts, alternative = "stationary")
acf(ERWMonthly_ts)
pacf(ERWMonthly_ts)

# run the arima function and search for best fit
auto.arima(ERWMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
ERWfit <- arima(ERWMonthly_ts, c(0, 1, 5),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
ERWprediction <- predict(ERWfit, n.ahead = 10*12)

# plot future predictions
ts.plot(ERWMonthly_ts, ERWprediction$pred, lty = c(1, 3))

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
names(MRODischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
MROPlot <- 
  ggplot(MRODischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MROPlot)

MRORegressionPlot <- 
  ggplot(MRODischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MRORegressionPlot)

MRODischarge <- na.omit(MRODischarge)
MRO_ts <- ts(MRODischarge[[4]], frequency = 365)
table(diff(MRODischarge$Date))

# Generate the decomposition
MRO_Decomposed <- stl(MRO_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(MRO_Decomposed)

# We can extract the components and turn them into data frames
MRO_Components <- as.data.frame(MRO_Decomposed$time.series[,1:3])
MRO_Components <- mutate(MRO_Components,
                         Observed = MRODischarge$Discharge,     
                         Date = MRODischarge$Date)

# Visualize how the trend maps onto the data
ggplot(MRO_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(MRO_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

MRODischarge.Monthly <- MRODischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MROMonthly_ts <- ts(MRODischarge.Monthly[[3]], frequency = 12)
adf.test(MROMonthly_ts, alternative = "stationary")
acf(MROMonthly_ts)
pacf(MROMonthly_ts)

# run the arima function and search for best fit
auto.arima(MROMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MROfit <- arima(MROMonthly_ts, c(2, 0, 1),seasonal = list(order = c(1, 1, 1), period = 12))

# make a prediction into the future
MROprediction <- predict(MROfit, n.ahead = 10*12)

# plot future predictions
ts.plot(MROMonthly_ts, MROprediction$pred, lty = c(1, 3))

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
names(BRLDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
BRLPlot <- 
  ggplot(BRLDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(BRLPlot)

BRLDischarge <- BRLDischarge %>%
  filter(Date > "1937-01-01") %>%
  arrange(Date)

# Replot discharge over time
BRLPlot <- 
  ggplot(BRLDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(BRLPlot)

BRLRegressionPlot <- 
  ggplot(BRLDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(BRLRegressionPlot)

BRLDischarge <- na.omit(BRLDischarge)
BRL_ts <- ts(BRLDischarge[[4]], frequency = 365)
table(diff(BRLDischarge$Date))

# Generate the decomposition
BRL_Decomposed <- stl(BRL_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(BRL_Decomposed)

# We can extract the components and turn them into data frames
BRL_Components <- as.data.frame(BRL_Decomposed$time.series[,1:3])
BRL_Components <- mutate(BRL_Components,
                         Observed = BRLDischarge$Discharge,     
                         Date = BRLDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(BRL_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(BRL_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

BRLDischarge.Monthly <- BRLDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
BRLMonthly_ts <- ts(BRLDischarge.Monthly[[3]], frequency = 12)
adf.test(BRLMonthly_ts, alternative = "stationary")
acf(BRLMonthly_ts)
pacf(BRLMonthly_ts)

# run the arima function and search for best fit
auto.arima(BRLMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
BRLfit <- arima(BRLMonthly_ts, c(1, 1, 2),seasonal = list(order = c(0, 0, 1), period = 12))

# make a prediction into the future
BRLprediction <- predict(BRLfit, n.ahead = 10*12)

# plot future predictions
ts.plot(BRLMonthly_ts, BRLprediction$pred, lty = c(1, 3))

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
names(NRHDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
NRHPlot <- 
  ggplot(NRHDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(NRHPlot)

NRHDischarge <- NRHDischarge %>%
  filter(Date > "1928-01-01") %>%
  arrange(Date)

# Replot discharge over time
NRHPlot <- 
  ggplot(NRHDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(NRHPlot)

NRHRegressionPlot <- 
  ggplot(NRHDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(NRHRegressionPlot)

NRHDischarge <- na.omit(NRHDischarge)
NRH_ts <- ts(NRHDischarge[[4]], frequency = 365)
table(diff(NRHDischarge$Date))

# Generate the decomposition
NRH_Decomposed <- stl(NRH_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(NRH_Decomposed)

# We can extract the components and turn them into data frames
NRH_Components <- as.data.frame(NRH_Decomposed$time.series[,1:3])
NRH_Components <- mutate(NRH_Components,
                         Observed = NRHDischarge$Discharge,     
                         Date = NRHDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(NRH_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(NRH_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

NRHDischarge.Monthly <- NRHDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
NRHMonthly_ts <- ts(NRHDischarge.Monthly[[3]], frequency = 12)
adf.test(NRHMonthly_ts, alternative = "stationary")
acf(NRHMonthly_ts)
pacf(NRHMonthly_ts)

# run the arima function and search for best fit
auto.arima(NRHMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
NRHfit <- arima(NRHMonthly_ts, c(1, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
NRHprediction <- predict(NRHfit, n.ahead = 10*12)

# plot future predictions
ts.plot(NRHMonthly_ts, NRHprediction$pred, lty = c(1, 3))

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
names(MRSJDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
MRSJPlot <- 
  ggplot(MRSJDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MRSJPlot)

MRSJRegressionPlot <- 
  ggplot(MRSJDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MRSJRegressionPlot)

MRSJDischarge <- na.omit(MRSJDischarge)
MRSJ_ts <- ts(MRSJDischarge[[4]], frequency = 365)
table(diff(MRSJDischarge$Date))

# Generate the decomposition
MRSJ_Decomposed <- stl(MRSJ_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(MRSJ_Decomposed)

# We can extract the components and turn them into data frames
MRSJ_Components <- as.data.frame(MRSJ_Decomposed$time.series[,1:3])
MRSJ_Components <- mutate(MRSJ_Components,
                         Observed = MRSJDischarge$Discharge,     
                         Date = MRSJDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(MRSJ_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(MRSJ_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

MRSJDischarge.Monthly <- MRSJDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MRSJMonthly_ts <- ts(MRSJDischarge.Monthly[[3]], frequency = 12)
adf.test(MRSJMonthly_ts, alternative = "stationary")
acf(MRSJMonthly_ts)
pacf(MRSJMonthly_ts)

# run the arima function and search for best fit
auto.arima(MRSJMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MRSJfit <- arima(MRSJMonthly_ts, c(2, 0, 1),seasonal = list(order = c(2, 1, 0), period = 12))

# make a prediction into the future
MRSJprediction <- predict(MRSJfit, n.ahead = 10*12)

# plot future predictions
ts.plot(MRSJMonthly_ts, MRSJprediction$pred, lty = c(1, 3))

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
names(RRODischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
RROPlot <- 
  ggplot(RRODischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(RROPlot)

RRORegressionPlot <- 
  ggplot(RRODischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(RRORegressionPlot)

RRODischarge <- na.omit(RRODischarge)
RRO_ts <- ts(RRODischarge[[4]], frequency = 365)
table(diff(RRODischarge$Date))

# Generate the decomposition
RRO_Decomposed <- stl(RRO_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(RRO_Decomposed)

# We can extract the components and turn them into data frames
RRO_Components <- as.data.frame(RRO_Decomposed$time.series[,1:3])
RRO_Components <- mutate(RRO_Components,
                          Observed = RRODischarge$Discharge,     
                          Date = RRODischarge$Date)

# Visualize how the trend maps onto the data
ggplot(RRO_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(RRO_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

RRODischarge.Monthly <- RRODischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
RROMonthly_ts <- ts(RRODischarge.Monthly[[3]], frequency = 12)
adf.test(RROMonthly_ts, alternative = "stationary")
acf(RROMonthly_ts)
pacf(RROMonthly_ts)

# run the arima function and search for best fit
auto.arima(RROMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
RROfit <- arima(RROMonthly_ts, c(1, 1, 1),seasonal = list(order = c(2, 0, 0), period = 12))

# make a prediction into the future
RROprediction <- predict(RROfit, n.ahead = 10*12)

# plot future predictions
ts.plot(RROMonthly_ts, RROprediction$pred, lty = c(1, 3))

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
names(RRCCDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
RRCCPlot <- 
  ggplot(RRCCDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(RRCCPlot)

RRCCRegressionPlot <- 
  ggplot(RRCCDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(RRCCRegressionPlot)

RRCCDischarge <- na.omit(RRCCDischarge)
RRCC_ts <- ts(RRCCDischarge[[4]], frequency = 365)
table(diff(RRCCDischarge$Date))

# Generate the decomposition
RRCC_Decomposed <- stl(RRCC_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(RRCC_Decomposed)

# We can extract the components and turn them into data frames
RRCC_Components <- as.data.frame(RRCC_Decomposed$time.series[,1:3])
RRCC_Components <- mutate(RRCC_Components,
                         Observed = RRCCDischarge$Discharge,     
                         Date = RRCCDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(RRCC_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(RRCC_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

RRCCDischarge.Monthly <- RRCCDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
RRCCMonthly_ts <- ts(RRCCDischarge.Monthly[[3]], frequency = 12)
adf.test(RRCCMonthly_ts, alternative = "stationary")
acf(RRCCMonthly_ts)
pacf(RRCCMonthly_ts)

# run the arima function and search for best fit
auto.arima(RRCCMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
RRCCfit <- arima(RRCCMonthly_ts, c(0, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
RRCCprediction <- predict(RRCCfit, n.ahead = 10*12)

# plot future predictions
ts.plot(RRCCMonthly_ts, RRCCprediction$pred, lty = c(1, 3))

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
names(SHEDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
SHEPlot <- 
  ggplot(SHEDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(SHEPlot)

SHERegressionPlot <- 
  ggplot(SHEDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(SHERegressionPlot)

SHEDischarge <- na.omit(SHEDischarge)
SHE_ts <- ts(SHEDischarge[[4]], frequency = 365)
table(diff(SHEDischarge$Date))

# Generate the decomposition
SHE_Decomposed <- stl(SHE_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(SHE_Decomposed)

# We can extract the components and turn them into data frames
SHE_Components <- as.data.frame(SHE_Decomposed$time.series[,1:3])
SHE_Components <- mutate(SHE_Components,
                          Observed = SHEDischarge$Discharge,     
                          Date = SHEDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(SHE_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(SHE_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

SHEDischarge.Monthly <- SHEDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
SHEMonthly_ts <- ts(SHEDischarge.Monthly[[3]], frequency = 12)
adf.test(SHEMonthly_ts, alternative = "stationary")
acf(SHEMonthly_ts)
pacf(SHEMonthly_ts)

# run the arima function and search for best fit
auto.arima(SHEMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
SHEfit <- arima(SHEMonthly_ts, c(1, 0, 1),seasonal = list(order = c(1, 0, 0), period = 12))

# make a prediction into the future
SHEprediction <- predict(SHEfit, n.ahead = 10*12)

# plot future predictions
ts.plot(SHEMonthly_ts, SHEprediction$pred, lty = c(1, 3))

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
names(SSODischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
SSOPlot <- 
  ggplot(SSODischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(SSOPlot)

SSORegressionPlot <- 
  ggplot(SSODischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(SSORegressionPlot)

SSODischarge <- na.omit(SSODischarge)
SSO_ts <- ts(SSODischarge[[4]], frequency = 365)
table(diff(SSODischarge$Date))

# Generate the decomposition
SSO_Decomposed <- stl(SSO_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(SSO_Decomposed)

# We can extract the components and turn them into data frames
SSO_Components <- as.data.frame(SSO_Decomposed$time.series[,1:3])
SSO_Components <- mutate(SSO_Components,
                         Observed = SSODischarge$Discharge,     
                         Date = SSODischarge$Date)

# Visualize how the trend maps onto the data
ggplot(SSO_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(SSO_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

SSODischarge.Monthly <- SSODischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
SSOMonthly_ts <- ts(SSODischarge.Monthly[[3]], frequency = 12)
adf.test(SSOMonthly_ts, alternative = "stationary")
acf(SSOMonthly_ts)
pacf(SSOMonthly_ts)

# run the arima function and search for best fit
auto.arima(SSOMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
SSOfit <- arima(SSOMonthly_ts, c(1, 0, 0))

# make a prediction into the future
SSOprediction <- predict(SSOfit, n.ahead = 10*12)

# plot future predictions
ts.plot(SSOMonthly_ts, SSOprediction$pred, lty = c(1, 3))

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
names(KWDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
KWPlot <- 
  ggplot(KWDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(KWPlot)

KWRegressionPlot <- 
  ggplot(KWDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(KWRegressionPlot)

KWDischarge <- na.omit(KWDischarge)
KW_ts <- ts(KWDischarge[[4]], frequency = 365)
table(diff(KWDischarge$Date))

# Generate the decomposition
KW_Decomposed <- stl(KW_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(KW_Decomposed)

# We can extract the components and turn them into data frames
KW_Components <- as.data.frame(KW_Decomposed$time.series[,1:3])
KW_Components <- mutate(KW_Components,
                         Observed = KWDischarge$Discharge,     
                         Date = KWDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(KW_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(KW_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

KWDischarge.Monthly <- KWDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
KWMonthly_ts <- ts(KWDischarge.Monthly[[3]], frequency = 12)
adf.test(KWMonthly_ts, alternative = "stationary")
acf(KWMonthly_ts)
pacf(KWMonthly_ts)

# run the arima function and search for best fit
auto.arima(KWMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
KWfit <- arima(KWMonthly_ts, c(1, 0, 0), seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
KWprediction <- predict(KWfit, n.ahead = 10*12)

# plot future predictions
ts.plot(KWMonthly_ts, KWprediction$pred, lty = c(1, 3))

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
names(KDDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
KDPlot <- 
  ggplot(KDDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(KDPlot)

KDRegressionPlot <- 
  ggplot(KDDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(KDRegressionPlot)

KDDischarge <- na.omit(KDDischarge)
KD_ts <- ts(KDDischarge[[4]], frequency = 365)
table(diff(KDDischarge$Date))

# Generate the decomposition
KD_Decomposed <- stl(KD_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(KD_Decomposed)

# We can extract the components and turn them into data frames
KD_Components <- as.data.frame(KD_Decomposed$time.series[,1:3])
KD_Components <- mutate(KD_Components,
                        Observed = KDDischarge$Discharge,     
                        Date = KDDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(KD_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(KD_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

KDDischarge.Monthly <- KDDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
KDMonthly_ts <- ts(KDDischarge.Monthly[[3]], frequency = 12)
adf.test(KDMonthly_ts, alternative = "stationary")
acf(KDMonthly_ts)
pacf(KDMonthly_ts)

# run the arima function and search for best fit
auto.arima(KDMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
KDfit <- arima(KDMonthly_ts, c(1, 0, 0), seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
KDprediction <- predict(KDfit, n.ahead = 10*12)

# plot future predictions
ts.plot(KDMonthly_ts, KDprediction$pred, lty = c(1, 3))

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
names(GRSDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
GRSPlot <- 
  ggplot(GRSDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(GRSPlot)

GRSRegressionPlot <- 
  ggplot(GRSDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(GRSRegressionPlot)

GRSDischarge <- na.omit(GRSDischarge)
GRS_ts <- ts(GRSDischarge[[4]], frequency = 365)
table(diff(GRSDischarge$Date))

# Generate the decomposition
GRS_Decomposed <- stl(GRS_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(GRS_Decomposed)

# We can extract the components and turn them into data frames
GRS_Components <- as.data.frame(GRS_Decomposed$time.series[,1:3])
GRS_Components <- mutate(GRS_Components,
                        Observed = GRSDischarge$Discharge,     
                        Date = GRSDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(GRS_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(GRS_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

GRSDischarge.Monthly <- GRSDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
GRSMonthly_ts <- ts(GRSDischarge.Monthly[[3]], frequency = 12)
adf.test(GRSMonthly_ts, alternative = "stationary")
acf(GRSMonthly_ts)
pacf(GRSMonthly_ts)

# run the arima function and search for best fit
auto.arima(GRSMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
GRSfit <- arima(GRSMonthly_ts, c(0, 1, 1), seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
GRSprediction <- predict(GRSfit, n.ahead = 10*12)

# plot future predictions
ts.plot(GRSMonthly_ts, GRSprediction$pred, lty = c(1, 3))

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
names(CRPDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
CRPPlot <- 
  ggplot(CRPDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(CRPPlot)

CRPRegressionPlot <- 
  ggplot(CRPDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(CRPRegressionPlot)

CRPDischarge <- na.omit(CRPDischarge)
CRP_ts <- ts(CRPDischarge[[4]], frequency = 365)
table(diff(CRPDischarge$Date))

# Generate the decomposition
CRP_Decomposed <- stl(CRP_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(CRP_Decomposed)

# We can extract the components and turn them into data frames
CRP_Components <- as.data.frame(CRP_Decomposed$time.series[,1:3])
CRP_Components <- mutate(CRP_Components,
                         Observed = CRPDischarge$Discharge,     
                         Date = CRPDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(CRP_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(CRP_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

CRPDischarge.Monthly <- CRPDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
CRPMonthly_ts <- ts(CRPDischarge.Monthly[[3]], frequency = 12)
adf.test(CRPMonthly_ts, alternative = "stationary")
acf(CRPMonthly_ts)
pacf(CRPMonthly_ts)

# run the arima function and search for best fit
auto.arima(CRPMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
CRPfit <- arima(CRPMonthly_ts, c(0, 1, 2), seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
CRPprediction <- predict(CRPfit, n.ahead = 10*12)

# plot future predictions
ts.plot(CRPMonthly_ts, CRPprediction$pred, lty = c(1, 3))

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
names(PTRPDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
PTRPPlot <- 
  ggplot(PTRPDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(PTRPPlot)

PTRPRegressionPlot <- 
  ggplot(PTRPDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(PTRPRegressionPlot)

PTRPDischarge <- na.omit(PTRPDischarge)
PTRP_ts <- ts(PTRPDischarge[[4]], frequency = 365)
table(diff(PTRPDischarge$Date))

# Generate the decomposition
PTRP_Decomposed <- stl(PTRP_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(PTRP_Decomposed)

# We can extract the components and turn them into data frames
PTRP_Components <- as.data.frame(PTRP_Decomposed$time.series[,1:3])
PTRP_Components <- mutate(PTRP_Components,
                         Observed = PTRPDischarge$Discharge,     
                         Date = PTRPDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(PTRP_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(PTRP_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

PTRPDischarge.Monthly <- PTRPDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
PTRPMonthly_ts <- ts(PTRPDischarge.Monthly[[3]], frequency = 12)
adf.test(PTRPMonthly_ts, alternative = "stationary")
acf(PTRPMonthly_ts)
pacf(PTRPMonthly_ts)

# run the arima function and search for best fit
auto.arima(PTRPMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
PTRPfit <- arima(PTRPMonthly_ts, c(1, 0, 0), seasonal = list(order = c(0, 0, 2), period = 12))

# make a prediction into the future
PTRPprediction <- predict(PTRPfit, n.ahead = 10*12)

# plot future predictions
ts.plot(PTRPMonthly_ts, PTRPprediction$pred, lty = c(1, 3))

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
names(ORSTDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
ORSTPlot <- 
  ggplot(ORSTDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(ORSTPlot)

ORSTDischarge <- ORSTDischarge %>%
  filter(Date > "2001-01-01") %>%
  arrange(Date)

# Replot discharge over time
ORSTPlot <- 
  ggplot(ORSTDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(ORSTPlot)

ORSTRegressionPlot <- 
  ggplot(ORSTDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(ORSTRegressionPlot)

ORSTDischarge <- na.omit(ORSTDischarge)
ORST_ts <- ts(ORSTDischarge[[4]], frequency = 365)
table(diff(ORSTDischarge$Date))

# Generate the decomposition
ORST_Decomposed <- stl(ORST_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(ORST_Decomposed)

# We can extract the components and turn them into data frames
ORST_Components <- as.data.frame(ORST_Decomposed$time.series[,1:3])
ORST_Components <- mutate(ORST_Components,
                          Observed = ORSTDischarge$Discharge,     
                          Date = ORSTDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(ORST_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(ORST_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

ORSTDischarge.Monthly <- ORSTDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
ORSTMonthly_ts <- ts(ORSTDischarge.Monthly[[3]], frequency = 12)
adf.test(ORSTMonthly_ts, alternative = "stationary")
acf(ORSTMonthly_ts)
pacf(ORSTMonthly_ts)

# run the arima function and search for best fit
auto.arima(ORSTMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
ORSTfit <- arima(ORSTMonthly_ts, c(1, 0, 0), seasonal = list(order = c(1, 0, 0), period = 12))

# make a prediction into the future
ORSTprediction <- predict(ORSTfit, n.ahead = 10*12)

# plot future predictions
ts.plot(ORSTMonthly_ts, ORSTprediction$pred, lty = c(1, 3))

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
names(LBRLCDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
LBRLCPlot <- 
  ggplot(LBRLCDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(LBRLCPlot)

LBRLCRegressionPlot <- 
  ggplot(LBRLCDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(LBRLCRegressionPlot)

LBRLCDischarge <- na.omit(LBRLCDischarge)
LBRLC_ts <- ts(LBRLCDischarge[[4]], frequency = 365)
table(diff(LBRLCDischarge$Date))

# Generate the decomposition
LBRLC_Decomposed <- stl(LBRLC_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(LBRLC_Decomposed)

# We can extract the components and turn them into data frames
LBRLC_Components <- as.data.frame(LBRLC_Decomposed$time.series[,1:3])
LBRLC_Components <- mutate(LBRLC_Components,
                          Observed = LBRLCDischarge$Discharge,     
                          Date = LBRLCDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(LBRLC_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(LBRLC_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

LBRLCDischarge.Monthly <- LBRLCDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
LBRLCMonthly_ts <- ts(LBRLCDischarge.Monthly[[3]], frequency = 12)
adf.test(LBRLCMonthly_ts, alternative = "stationary")
acf(LBRLCMonthly_ts)
pacf(LBRLCMonthly_ts)

# run the arima function and search for best fit
auto.arima(LBRLCMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
LBRLCfit <- arima(LBRLCMonthly_ts, c(0, 1, 2), seasonal = list(order = c(0, 0, 1), period = 12))

# make a prediction into the future
LBRLCprediction <- predict(LBRLCfit, n.ahead = 10*12)

# plot future predictions
ts.plot(LBRLCMonthly_ts, LBRLCprediction$pred, lty = c(1, 3))

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
names(MRHDischarge)[4:5] <-c("Discharge", "Approval.Code")

# Plot discharge over time
MRHPlot <- 
  ggplot(MRHDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MRHPlot)

MRHRegressionPlot <- 
  ggplot(MRHDischarge, aes(x = Date, y = Discharge)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#c13d75ff") +
  labs(x = "", y = expression("Discharge (ft"^3*"/s)")) + 
  theme(plot.title = element_text(margin = margin(b = -10), size = 12), 
        axis.title.x = element_blank())
print(MRHRegressionPlot)

MRHDischarge <- na.omit(MRHDischarge)
MRH_ts <- ts(MRHDischarge[[4]], frequency = 365)
table(diff(MRHDischarge$Date))

# Generate the decomposition
MRH_Decomposed <- stl(MRH_ts, s.window = "periodic")

# Visualize the decomposed series. 
plot(MRH_Decomposed)

# We can extract the components and turn them into data frames
MRH_Components <- as.data.frame(MRH_Decomposed$time.series[,1:3])
MRH_Components <- mutate(MRH_Components,
                           Observed = MRHDischarge$Discharge,     
                           Date = MRHDischarge$Date)

# Visualize how the trend maps onto the data
ggplot(MRH_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = trend, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

# Visualize how the seasonal cycle maps onto the data
ggplot(MRH_Components) +
  geom_line(aes(y = Observed, x = Date),  size = 0.25) +
  geom_line(aes(y = seasonal, x = Date), color = "#c13d75ff") +
  geom_hline(yintercept = 0, lty = 2) +
  ylab(expression("Discharge (ft"^3*"/s)"))

MRHDischarge.Monthly <- MRHDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MRHMonthly_ts <- ts(MRHDischarge.Monthly[[3]], frequency = 12)
adf.test(MRHMonthly_ts, alternative = "stationary")
acf(MRHMonthly_ts)
pacf(MRHMonthly_ts)

# run the arima function and search for best fit
auto.arima(MRHMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MRHfit <- arima(MRHMonthly_ts, c(5, 1, 0), seasonal = list(order = c(2, 0, 0), period = 12))

# make a prediction into the future
MRHprediction <- predict(MRHfit, n.ahead = 10*12)

# plot future predictions
ts.plot(MRHMonthly_ts, MRHprediction$pred, lty = c(1, 3))

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
