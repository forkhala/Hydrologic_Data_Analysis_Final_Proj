## Session Set Up
getwd()

library(tidyverse)
library(dataRetrieval)
library(cowplot)
library(EcoHydRology)
library(xts)
library(dygraphs)

theme_set(theme_classic())

# Pull data
highfreqsites.DN <- readNWISuv(site = c("06808500", "06817000", "06892350","06892513", "06902000", "06934500"), 
                               parameterCd = c("00060"), 
                               # Discharge in cfs
                               startDate = "2019-03-01",
                               endDate = "2019-08-01") %>%
  renameNWISColumns()

#individual sites
Randolph <- highfreqsites.DN %>%
  filter(site_no=="06808500")
Randolph <- na.omit(Randolph)

Clarinda <- highfreqsites.DN %>%
  filter(site_no=="06817000")
Clarinda <- na.omit(Clarinda)

Desoto <- highfreqsites.DN %>%
  filter(site_no=="06892350")
Desoto <- na.omit(Desoto)

Shawnee <- highfreqsites.DN %>%
  filter(site_no=="06892513")
Shawnee <- na.omit(Shawnee)

Sumner <- highfreqsites.DN %>%
  filter(site_no=="06902000")
Sumner <- na.omit(Sumner)

Hermann <- highfreqsites.DN %>%
  filter(site_no=="06934500")
Hermann <- na.omit(Hermann)

plot_grid(
  ggplot(Randolph, aes(x = dateTime, y = Flow_Inst)) +
    geom_line() + 
    scale_y_log10() +
    ggtitle("West Nishnabotna River at Randolph, IA"),
  ggplot(Clarinda, aes(x = dateTime, y = Flow_Inst)) + 
    geom_line() + 
    scale_y_log10() +
    ggtitle("Nodaway River at Clarinda, IA"),
  ggplot(Desoto, aes(x = dateTime, y = Flow_Inst)) + 
    geom_line() + 
    scale_y_log10() +
    ggtitle("KANSAS R AT DESOTO, KS"),
  ggplot(Shawnee, aes(x = dateTime, y = Flow_Inst)) + 
    geom_line() + 
    scale_y_log10() +
    ggtitle("MILL C AT JOHNSON DRIVE, SHAWNEE, KS"),
  ggplot(Sumner, aes(x = dateTime, y = Flow_Inst)) + 
    geom_line() + 
    scale_y_log10() +
    ggtitle("Grand River near Sumner, MO"),
  ggplot(Hermann, aes(x = dateTime, y = Flow_Inst)) + 
    geom_line() + 
    scale_y_log10() +
    ggtitle("Missouri River at Hermann, MO"),
  ncol = 1
)

############################## Randolph ###################################
# Hydrograph separation
Randolphbaseflow <- BaseflowSeparation(
  Randolph$Flow_Inst, 
  filter_parameter = 0.925, 
  passes = 3
)

Randolph2018 <- cbind(Randolph, Randolphbaseflow)

ggplot(Randolph2018, aes(x = dateTime, y = Flow_Inst)) + 
  geom_line() +
  geom_line(mapping = aes(x = dateTime, y = bt), color = "darkorange4") +
  geom_line(mapping = aes(x = dateTime, y = qft), color = "steelblue4")

dygraph(
  cbind(
    Flow = with(Randolph2018, xts(Flow_Inst, order.by = dateTime)), 
    Baseflow = with(Randolph2018, xts(bt, order.by = dateTime)), 
    Quickflow = with(Randolph2018, xts(qft, order.by = dateTime))
  )
) %>% 
  dyRangeSelector()

RandolphExport <- Randolph2018 %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_),
         baseflowexport = bt * timestep,
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T),
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

# Hydrologic Flashiness
Randolphdata <- readNWISsite(site = "06808500")
Randolph.catchment.size <- Randolphdata$drain_area_va #square miles

RBI_Randolph2018 <- (
  sum(abs(diff(Randolph$Flow_Inst))) / 
    sum(Randolph$Flow_Inst[-1])
) / Randolph.catchment.size

############################## Clarinda ###################################
# Hydrograph separation
Clarindabaseflow <- BaseflowSeparation(
  Clarinda$Flow_Inst, 
  filter_parameter = 0.925, 
  passes = 3
)

Clarinda2018 <- cbind(Clarinda, Clarindabaseflow)

ggplot(Clarinda2018, aes(x = dateTime, y = Flow_Inst)) + 
  geom_line() +
  geom_line(mapping = aes(x = dateTime, y = bt), color = "darkorange4") +
  geom_line(mapping = aes(x = dateTime, y = qft), color = "steelblue4")

dygraph(
  cbind(
    Flow = with(Clarinda2018, xts(Flow_Inst, order.by = dateTime)), 
    Baseflow = with(Clarinda2018, xts(bt, order.by = dateTime)), 
    Quickflow = with(Clarinda2018, xts(qft, order.by = dateTime))
  )
) %>% 
  dyRangeSelector()

ClarindaExport <- Clarinda2018 %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_),
         baseflowexport = bt * timestep,
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T),
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

# Hydrologic Flashiness
Clarindadata <- readNWISsite(site = "06817000")
Clarinda.catchment.size <- Clarindadata$drain_area_va #square miles

RBI_Clarinda2018 <- (
  sum(abs(diff(Clarinda$Flow_Inst))) / 
    sum(Clarinda$Flow_Inst[-1])
) / Clarinda.catchment.size

############################## Desoto ###################################
# Hydrograph separation
Desotobaseflow <- BaseflowSeparation(
  Desoto$Flow_Inst, 
  filter_parameter = 0.925, 
  passes = 3
)

Desoto2018 <- cbind(Desoto, Desotobaseflow)

ggplot(Desoto2018, aes(x = dateTime, y = Flow_Inst)) + 
  geom_line() +
  geom_line(mapping = aes(x = dateTime, y = bt), color = "darkorange4") +
  geom_line(mapping = aes(x = dateTime, y = qft), color = "steelblue4")

dygraph(
  cbind(
    Flow = with(Desoto2018, xts(Flow_Inst, order.by = dateTime)), 
    Baseflow = with(Desoto2018, xts(bt, order.by = dateTime)), 
    Quickflow = with(Desoto2018, xts(qft, order.by = dateTime))
  )
) %>% 
  dyRangeSelector()

DesotoExport <- Desoto2018 %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_),
         baseflowexport = bt * timestep,
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T),
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

# Hydrologic Flashiness
Desotodata <- readNWISsite(site = "06892350")
Desoto.catchment.size <- Desotodata$drain_area_va #square miles

RBI_Desoto2018 <- (
  sum(abs(diff(Desoto$Flow_Inst))) / 
    sum(Desoto$Flow_Inst[-1])
) / Desoto.catchment.size

############################## Shawnee ###################################
# Hydrograph separation
Shawneebaseflow <- BaseflowSeparation(
  Shawnee$Flow_Inst, 
  filter_parameter = 0.925, 
  passes = 3
)

Shawnee2018 <- cbind(Shawnee, Shawneebaseflow)

ggplot(Shawnee2018, aes(x = dateTime, y = Flow_Inst)) + 
  geom_line() +
  geom_line(mapping = aes(x = dateTime, y = bt), color = "darkorange4") +
  geom_line(mapping = aes(x = dateTime, y = qft), color = "steelblue4")

dygraph(
  cbind(
    Flow = with(Shawnee2018, xts(Flow_Inst, order.by = dateTime)), 
    Baseflow = with(Shawnee2018, xts(bt, order.by = dateTime)), 
    Quickflow = with(Shawnee2018, xts(qft, order.by = dateTime))
  )
) %>% 
  dyRangeSelector()

ShawneeExport <- Shawnee2018 %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_),
         baseflowexport = bt * timestep,
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T),
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

# Hydrologic Flashiness
Shawneedata <- readNWISsite(site = "06892513")
Shawnee.catchment.size <- Shawneedata$drain_area_va #square miles

RBI_Shawnee2018 <- (
  sum(abs(diff(Shawnee$Flow_Inst))) / 
    sum(Shawnee$Flow_Inst[-1])
) / Shawnee.catchment.size

############################## Sumner ###################################
# Hydrograph separation
Sumnerbaseflow <- BaseflowSeparation(
  Sumner$Flow_Inst, 
  filter_parameter = 0.925, 
  passes = 3
)

Sumner2018 <- cbind(Sumner, Sumnerbaseflow)

ggplot(Sumner2018, aes(x = dateTime, y = Flow_Inst)) + 
  geom_line() +
  geom_line(mapping = aes(x = dateTime, y = bt), color = "darkorange4") +
  geom_line(mapping = aes(x = dateTime, y = qft), color = "steelblue4")

dygraph(
  cbind(
    Flow = with(Sumner2018, xts(Flow_Inst, order.by = dateTime)), 
    Baseflow = with(Sumner2018, xts(bt, order.by = dateTime)), 
    Quickflow = with(Sumner2018, xts(qft, order.by = dateTime))
  )
) %>% 
  dyRangeSelector()

SumnerExport <- Sumner2018 %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_),
         baseflowexport = bt * timestep,
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T),
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

# Hydrologic Flashiness
Sumnerdata <- readNWISsite(site = "06902000")
Sumner.catchment.size <- Sumnerdata$drain_area_va #square miles

RBI_Sumner2018 <- (
  sum(abs(diff(Sumner$Flow_Inst))) / 
    sum(Sumner$Flow_Inst[-1])
) / Sumner.catchment.size

############################## Hermann ###################################
# Hydrograph separation
Hermannbaseflow <- BaseflowSeparation(
  Hermann$Flow_Inst, 
  filter_parameter = 0.925, 
  passes = 3
)

Hermann2018 <- cbind(Hermann, Hermannbaseflow)

ggplot(Hermann2018, aes(x = dateTime, y = Flow_Inst)) + 
  geom_line() +
  geom_line(mapping = aes(x = dateTime, y = bt), color = "darkorange4") +
  geom_line(mapping = aes(x = dateTime, y = qft), color = "steelblue4")

dygraph(
  cbind(
    Flow = with(Hermann2018, xts(Flow_Inst, order.by = dateTime)), 
    Baseflow = with(Hermann2018, xts(bt, order.by = dateTime)), 
    Quickflow = with(Hermann2018, xts(qft, order.by = dateTime))
  )
) %>% 
  dyRangeSelector()

HermannExport <- Hermann2018 %>%
  mutate(timestep = c(diff(as.numeric(dateTime)), NA_real_),
         baseflowexport = bt * timestep,
         quickflowexport = qft * timestep) %>%
  summarize(BaseflowExport_cf = sum(baseflowexport, na.rm = T),
            QuickflowExport_cf = sum(quickflowexport, na.rm = T),
            TotalExport_cf = BaseflowExport_cf + QuickflowExport_cf)

# Hydrologic Flashiness
Hermanndata <- readNWISsite(site = "06934500")
Hermann.catchment.size <- Hermanndata$drain_area_va #square miles

RBI_Hermann2018 <- (
  sum(abs(diff(Hermann$Flow_Inst))) / 
    sum(Hermann$Flow_Inst[-1])
) / Hermann.catchment.size