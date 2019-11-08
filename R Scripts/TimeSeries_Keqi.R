## Session Set Up
getwd()

library(tidyverse)
library(dataRetrieval)

theme_set(theme_classic())

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

########################### Nutrition Analysis ############################

PlatteRiverNitrogen <- readNWISqw(siteNumbers = "06768000",
                                   parameterCd = "00600", # nitrogen (mg/L)
                                   startDate = "",
                                   endDate = "")
