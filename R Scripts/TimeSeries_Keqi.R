## Session Set Up
getwd()

library(tidyverse)
library(dataRetrieval)
library(trend)
library(forecast)
library(tseries)

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
  filter(Date > "1937-01-01") %>%
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
NRHfit <- arima(NRHMonthly_ts, c(1, 1, 2),seasonal = list(order = c(0, 0, 1), period = 12))

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
  filter(result_va < 30) %>%
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
                     start = c(2004, 3, 12), end = c(2014, 9, 15))
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
  filter(result_va < 5) %>%
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
                     start = c(2004, 3, 12), end = c(2014, 9, 15))
# Run SMK test
NRHPtrend <- smk.test(NRHPtimeseries)

# Inspect results
NRHPtrend
summary(NRHPtrend)