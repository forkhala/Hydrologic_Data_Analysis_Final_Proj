
########## Discharge Analysis ##########

######## Setup########

# Load Required Packages
pacman::p_load(tidyverse, dataRetrieval, lubridate, cowplot, grid, gridExtra, ggpubr)
theme_set(theme_classic())

# Import site info ####

site.list <- read_csv("./Data/Processed/bestsiteslists.csv", col_types = cols(
  X1 = "d", site_no = "c", site_nm = "c", huc_cd = "c", huc4 = "c", huc4_nm = "c"))%>%
  arrange(huc4, huc_cd)

site.nos <- site.list$site_no
#----setup end ----

########## Discharge Pattern in a Year ##########

# Following codes for specific watersheds are only for validation; use loop to analyze all

######## HU 1020 Platte ########

##### 10200101 Middle Platte-Buffalo #####

# Import data from NWIS
hu10200101 <- readNWISdv(siteNumbers = site.nos[1], 
                          parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
  rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)

hu10200101.pattern <- hu10200101 %>%
  mutate(DOY = day(Date)) %>%
  group_by(DOY) %>%
  summarise(Median.Discharge = median(Discharge), 
            Max.Discharge = max(Discharge),
            Min.Discharge = min(Discharge))
fig10200101.1 <- ggplot(hu10200101.pattern, aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge)) +
  geom_line(aes(y = Max.Discharge), color = "gray") +
  geom_line(aes(y = Min.Discharge), color = "gray") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) 
print(fig10200101.1)

#---- 10200101 ----

##### 10200202 Lower Platte #####

hu10200202 <- readNWISdv(siteNumbers = site.nos[2], 
                         parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
  rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)

hu10200202.pattern <- hu10200202 %>%
  mutate(DOY = day(Date)) %>%
  group_by(DOY) %>%
  summarise(Median.Discharge = median(Discharge), 
            Max.Discharge = max(Discharge),
            Min.Discharge = min(Discharge))
fig10200202.1 <- ggplot(hu10200202.pattern, aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge)) +
  geom_line(aes(y = Max.Discharge), color = "gray") +
  geom_line(aes(y = Min.Discharge), color = "gray") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) 
print(fig10200202.1)

#---- 10200202 end ----

fig1020 <- ggplot(hu10200101.pattern, mapping = aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge), color = "lightskyblue4", size = 0.73) +
  geom_line(aes(y = Max.Discharge), color = "lightskyblue3") +
  geom_line(aes(y = Min.Discharge), color = "lightskyblue3") +  
  geom_line(aes(y = Median.Discharge), hu10200202.pattern, color = "darkseagreen4", size = 0.73)+
  geom_line(aes(y = Max.Discharge), hu10200202.pattern, color = "darkseagreen3") +
  geom_line(aes(y = Min.Discharge), hu10200202.pattern, color = "darkseagreen3") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) +
  annotate("text", x = -Inf, y = Inf, label = "1020", hjust = 0, vjust = 1, size = 4.5)
print(fig1020)

######## HU 1020 Platte ########

##### 10200101 Middle Platte-Buffalo #####

# Import data from NWIS
hu10200101 <- readNWISdv(siteNumbers = site.nos[1], 
                         parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
  rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)

hu10200101.pattern <- hu10200101 %>%
  mutate(DOY = yday(Date)) %>%
  group_by(DOY) %>%
  summarise(Median.Discharge = median(Discharge), 
            Max.Discharge = max(Discharge),
            Min.Discharge = min(Discharge))
fig10200101.1 <- ggplot(hu10200101.pattern, aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge)) +
  geom_line(aes(y = Max.Discharge), color = "gray") +
  geom_line(aes(y = Min.Discharge), color = "gray") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) 
print(fig10200101.1)

#---- 10200101 ----

##### 10200202 Lower Platte #####

hu10200202 <- readNWISdv(siteNumbers = site.nos[2], 
                         parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
  rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)

hu10200202.pattern <- hu10200202 %>%
  mutate(DOY = yday(Date)) %>%
  group_by(DOY) %>%
  summarise(Median.Discharge = median(Discharge), 
            Max.Discharge = max(Discharge),
            Min.Discharge = min(Discharge))
fig10200202.1 <- ggplot(hu10200202.pattern, aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge)) +
  geom_line(aes(y = Max.Discharge), color = "gray") +
  geom_line(aes(y = Min.Discharge), color = "gray") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) 
print(fig10200202.1)

#---- 10200202 end ----

##### Combine the two 1020 sites #####

fig1020 <- ggplot(hu10200101.pattern, mapping = aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge), color = "lightskyblue4", size = 0.73) +
  geom_line(aes(y = Max.Discharge), color = "lightskyblue3") +
  geom_line(aes(y = Min.Discharge), color = "lightskyblue3") +  
  geom_line(aes(y = Median.Discharge), hu10200202.pattern, color = "darkseagreen4", size = 0.73)+
  geom_line(aes(y = Max.Discharge), hu10200202.pattern, color = "darkseagreen3") +
  geom_line(aes(y = Min.Discharge), hu10200202.pattern, color = "darkseagreen3") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) 
print(fig1020)

#---- Combining end ----

######## for loop ########
##### The codes above are only for validation; use loop below to generate all figures

for(i in 1:11){
  site1 <- readNWISdv(siteNumbers = site.nos[2*i-1], 
                           parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
    rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  site1.pattern <- site1 %>%
    mutate(DOY = yday(Date)) %>%
    group_by(DOY) %>%
    summarise(Median.Discharge = median(Discharge), 
              Max.Discharge = max(Discharge),
              Min.Discharge = min(Discharge))

  site2 <- readNWISdv(siteNumbers = site.nos[2*i], 
                           parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
  rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  site2.pattern <- site2 %>%
    mutate(DOY = yday(Date)) %>%
    group_by(DOY) %>%
    summarise(Median.Discharge = median(Discharge), 
              Max.Discharge = max(Discharge),
              Min.Discharge = min(Discharge))

  fig <- ggplot(site1.pattern, mapping = aes(x = DOY)) +
    geom_line(aes(y = Median.Discharge), color = "lightskyblue4", size = 0.73) +
    geom_line(aes(y = Max.Discharge), color = "lightskyblue3", alpha = 0.8) +
    geom_line(aes(y = Min.Discharge), color = "lightskyblue3", alpha = 0.8) +  
    geom_line(aes(y = Median.Discharge),site2.pattern, color = "darkseagreen4",size = 0.73)+
    geom_line(aes(y = Max.Discharge), site2.pattern, color = "darkseagreen3", alpha = 0.8) +
    geom_line(aes(y = Min.Discharge), site2.pattern, color = "darkseagreen3", alpha = 0.8) + 
    annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1, size = 3,
             label = paste(site.list$huc4[2*i], site.list$huc4_nm[2*i], sep = " "))+
    labs(x = element_blank(), y = element_blank())
  # Rename outputs
  assign(paste0("figure", site.list$huc4[2*i]), fig)
  #assign(paste0("dat", site.list$huc4[2*i-1], ".1"), site1)
  #assign(paste0("dat", site.list$huc4[2*i], ".2"), site2)
  
  rm(site1, site1.pattern, site2, site2.pattern, fig)
}

##### Combine all discharge plots #####

DischargePlot <- ggarrange(DischargePlot1020,DischargePlot1021,DischargePlot1022,DischargePlot1023,
                           DischargePlot1024,DischargePlot1025,DischargePlot1026,DischargePlot1027,
                           DischargePlot1028,DischargePlot1029,DischargePlot1030 
                           + font("x.text", size = 10), ncol = 4, nrow = 3)
annotated.DisChargePlot <-
  annotate_figure(DischargePlot,bottom=text_grob("Day of Year",color="black",hjust=0.5, size=13),
                left = text_grob(expression("Discharge ft"^3*" s"^-1), 
                                 color = "black", rot = 90, size = 13))
ggsave("./Figures/discharge.jpg", annotated.DisChargePlot, 
       dpi = 300, width = 14, height = 9, units = "in")

#---- end ----


########## Recurrence Interval ##########

######## HU 1020 Platte (for validation) ########


Recurrence1020.1 <- 
  dat1020.1 %>%
  mutate(Year = year(Date))%>%
  group_by(Year) %>%
  summarise(PeakDischarge = max(Discharge)) %>% 
  mutate(Rank = rank(-PeakDischarge),
         RecurrenceInterval = (length(Year) + 1)/Rank, 
         Probability = 1/RecurrenceInterval)

PeakPlot.1020.1 <- 
  ggplot(Recurrence1020.1, aes(x = Year, y = PeakDischarge)) +
  geom_bar(stat = "identity") +
  xlab("Year")
print(PeakPlot.1020.1)

RecurrencePlot.1020.1 <- ggplot() +
  geom_point(data = Recurrence1020.1, aes(x = RecurrenceInterval, y = PeakDischarge),
             color = "lightskyblue4", size = 1, alpha = 0.7)+
  stat_smooth(data = Recurrence1020.1, aes(x = RecurrenceInterval, y = PeakDischarge),
              method = "lm" , formula = y ~ log(x), 
              level = 0.95, alpha = 0.3, size = 0.75, 
              color = "lightskyblue4", fill = "lightskyblue3") #+
#scale_x_log10() 
print(RecurrencePlot.1020.1)

# fit model
model.1020.1 <- lm(data = Recurrence1020.1, PeakDischarge ~ log(RecurrenceInterval))
summary(model.1020.1)
# par(mfrow = c(2,2), mai = c(0.7,0.7,0.7,0.7)); plot(model.1020.1)

# Use custom fitted values and CI 
newdat <- data.frame(
  RecurrenceInterval = seq(min(Recurrence1020.1$RecurrenceInterval), 
                           max(Recurrence1020.1$RecurrenceInterval), length.out = 100))

newdat[,2:4] <- predict.lm(model.1020.1, newdat, interval = "c", level = 0.95)
names(newdat)[2:4] <- c("fit", "lwr", "upr")

ggplot()+
  geom_point(dat = Recurrence1020.1, aes(x = RecurrenceInterval, y = PeakDischarge))+
  geom_ribbon(data = newdat, aes(x = RecurrenceInterval, ymin = lwr, ymax = upr), 
              fill = "gray70", alpha = 0.5)+
  geom_line(data = newdat, aes(x = RecurrenceInterval, y = fit))

#----1020 end----

######## for loop ########

for (i in 1:11) {
  site1 <- readNWISdv(siteNumbers = site.nos[2*i-1], 
                      parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
    rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  Recurrence.site1 <- site1 %>%
    mutate(Year = year(Date))%>%
    group_by(Year) %>%
    summarise(PeakDischarge = max(Discharge)) %>% 
    mutate(Rank = rank(-PeakDischarge),
           RecurrenceInterval = (length(Year) + 1)/Rank, 
           Probability = 1/RecurrenceInterval)
  
  site2 <- readNWISdv(siteNumbers = site.nos[2*i], 
                      parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
    rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  Recurrence.site2 <- site2 %>%
    mutate(Year = year(Date))%>%
    group_by(Year) %>%
    summarise(PeakDischarge = max(Discharge)) %>% 
    mutate(Rank = rank(-PeakDischarge),
           RecurrenceInterval = (length(Year) + 1)/Rank, 
           Probability = 1/RecurrenceInterval)
  
  model.1 <- lm(data = Recurrence.site1, PeakDischarge ~ log(RecurrenceInterval))
  model.2 <- lm(data = Recurrence.site1, PeakDischarge ~ log(RecurrenceInterval))
  model.1.sum <- summary(model.1)
  model.2.sum <- summary(model.2)
  
  fig <- ggplot() +
    geom_point(data = Recurrence.site1, aes(x = RecurrenceInterval, y = PeakDischarge),
               color = "lightskyblue4", size = 1, alpha = 0.7)+
    stat_smooth(data = Recurrence.site1, aes(x = RecurrenceInterval, y = PeakDischarge),
                method = "lm" , formula = y ~ log(x), 
                level = 0.95, alpha = 0.3, size = 0.75, 
                color = "lightskyblue4", fill = "lightskyblue3") +
    geom_point(data = Recurrence.site2, aes(x = RecurrenceInterval, y = PeakDischarge), 
               color = "darkseagreen4", size = 1, alpha = 0.7)+
    stat_smooth(data = Recurrence.site2, aes(x = RecurrenceInterval, y = PeakDischarge),
                method = "lm" , formula = y ~ log(x), 
                level = 0.95, alpha = 0.4, size = 0.75, 
                color = "darkseagreen4", fill = "darkseagreen3")+
    annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1, size = 3,
             label = paste(site.list$huc4[2*i], site.list$huc4_nm[2*i], sep = " "))+
    labs(x = element_blank(), y = element_blank())
    
  assign(paste0("RecPlot", site.list$huc4[2*i-1]), fig)
  assign(paste0("mod.", site.list$huc_cd[2*i-1]), model.1)
  assign(paste0("mod.", site.list$huc_cd[2*i]), model.2)
  assign(paste0("mod.", site.list$huc_cd[2*i-1]), model.1.sum)
  assign(paste0("mod.", site.list$huc_cd[2*i]), model.2.sum)
  rm(site1, site2, Recurrence.site1, Recurrence.site2,
     fig, model.1, model.2, model.1.sum, model.2.sum)
}

##### Combine all discharge plots #####

RecPlot <- ggarrange(RecPlot1020, RecPlot1021, RecPlot1022, RecPlot1023, RecPlot1024, RecPlot1025, 
                    RecPlot1026, RecPlot1027, RecPlot1028, RecPlot1029, 
                    RecPlot1030 + font("x.text", size = 10), ncol = 4, nrow = 3)
annotated.RecPlot <- 
  annotate_figure(RecPlot, bottom=text_grob("Recurrence",color = "black",hjust = 0.5,size = 13),
                  left = text_grob(expression("Discharge ft"^3*" s"^-1), color = "black",
                                   rot = 90, size = 13))

ggsave("./Figures/recurrence.jpg", annotated.RecPlot,
       dpi = 300, width = 14, height = 9, units = "in")
