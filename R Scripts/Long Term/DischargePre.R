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
############## 22 Missouri River at Hermann, MO ###########################
###########################################################################

########################### Discharge Analysis ############################

MRHDischarge <- readNWISdv(siteNumbers = "06934500",
                           parameterCd = "00060", # discharge (ft3/s)
                           startDate = "",
                           endDate = "")
names(MRHDischarge)[4:5] <-c("Discharge", "Approval.Code")

MRHDischarge <- na.omit(MRHDischarge)
MRH_ts <- ts(MRHDischarge[[4]], frequency = 365)
table(diff(MRHDischarge$Date))

MRHDischarge.Monthly <- MRHDischarge %>%
  mutate(Year = year(Date),
         Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Discharge = mean(Discharge))
MRHMonthly_ts <- ts(MRHDischarge.Monthly[[3]], frequency = 12)
adf.test(MRHMonthly_ts, alternative = "stationary")
# acf(MRHMonthly_ts)
# pacf(MRHMonthly_ts)

# run the arima function and search for best fit
# auto.arima(MRHMonthly_ts, trace = TRUE)

# create an object that defines the best fit model
MRHfit <- arima(MRHMonthly_ts, c(5, 1, 0), seasonal = list(order = c(2, 0, 0), period = 12))

# make a prediction into the future
MRHprediction <- predict(MRHfit, n.ahead = 10*12)

# plot future predictions
ts.plot(MRHMonthly_ts/10^5, MRHprediction$pred/10^5, lty = c(1, 3))
