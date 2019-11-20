
########## Discharge Analysis ##########

######## Setup########

# Load Required Packages
pacman::p_load(tidyverse, dataRetrieval, lubridate, cowplot, grid, gridExtra, ggpubr)
theme_set(theme_classic())

# Import site info ####

site.list <- read_csv("./Data/Processed/bestsiteslist.csv", col_types = cols(
  site_no = "c",site_nm = "c", huc_cd = "c", huc4 = "c", huc4_nm = "c", site_lab = "c"))%>%
  arrange(huc4, huc_cd)

site.nos <- site.list$site_no
#----setup end ----

######################### Part I Discharge Pattern in a Year #########################

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
pb <- winProgressBar(title="Loop in progress", label="0% done", 
                     min=0, max=100, initial=0)
for(i in 1:11){
  site1 <- readNWISdv(siteNumbers = site.nos[2*i-1], 
                           parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
    rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  site1.pattern <- site1 %>%
    mutate(DOY = yday(Date)) %>%
    group_by(DOY) %>%
    summarise(Median.Discharge = median(Discharge), 
              Max.Discharge = max(Discharge),
              Min.Discharge = min(Discharge),
              Discharge75 = quantile(Discharge, probs = 0.75, na.rm = T),
              Discharge25 = quantile(Discharge, probs = 0.25, na.rm = T))

  site2 <- readNWISdv(siteNumbers = site.nos[2*i], 
                           parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
  rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  site2.pattern <- site2 %>%
    mutate(DOY = yday(Date)) %>%
    group_by(DOY) %>%
    summarise(Median.Discharge = median(Discharge), 
              Max.Discharge = max(Discharge),
              Min.Discharge = min(Discharge),
              Discharge75 = quantile(Discharge, probs = 0.75, na.rm = T),
              Discharge25 = quantile(Discharge, probs = 0.25, na.rm = T))

  fig <- ggplot(site1.pattern, mapping = aes(x = DOY)) +
    geom_line(aes(y = Median.Discharge), color = "lightskyblue4", size = 0.73) +
    geom_line(aes(y = Discharge75), color = "lightskyblue3", alpha = 0.8, linetype = 6) +
    geom_line(aes(y = Discharge25), color = "lightskyblue3", alpha = 0.8, linetype = 6) +  
    geom_line(aes(y = Median.Discharge),site2.pattern, color = "darkseagreen4",size = 0.73)+
    geom_line(aes(y = Discharge75),site2.pattern,color = "darkseagreen3",alpha = 0.8,linetype = 6)+
    geom_line(aes(y = Discharge25),site2.pattern,color = "darkseagreen3",alpha = 0.8,linetype = 6)+ 
    annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1, size = 3,
             label = paste(site.list$huc4[2*i], site.list$huc4_nm[2*i], sep = " "))+
    labs(x = element_blank(), y = element_blank())
  # Rename outputs
  assign(paste0("DischargePlot", site.list$huc4[2*i]), fig)
  #assign(paste0("dat", site.list$huc4[2*i-1], ".1"), site1)
  #assign(paste0("dat", site.list$huc4[2*i], ".2"), site2)
  
  rm(site1, site1.pattern, site2, site2.pattern, fig)
  
  # output info on progress
  prog <- sprintf("%d%% done", round((i/11)*100))
  setWinProgressBar(pb, i/(11)*100, label=prog)
}
close(pb)
##### Combine all discharge plots #####

DischargePlot <- ggarrange(DischargePlot1020,DischargePlot1021,DischargePlot1022,DischargePlot1023,
                           DischargePlot1024,DischargePlot1025,DischargePlot1026,DischargePlot1027,
                           DischargePlot1028,DischargePlot1029,DischargePlot1030 
                           + font("x.text", size = 10), ncol = 4, nrow = 3)
annotated.DisChargePlot <-
  annotate_figure(DischargePlot,bottom=text_grob("Day of Year",color="black",hjust=0.5, size=13),
                left = text_grob(expression("Discharge ft"^3*" s"^-1), 
                                 color = "black", rot = 90, size = 13))
ggsave("./Figures/discharge.png", annotated.DisChargePlot, 
       dpi = 300, width = 14, height = 9, units = "in")

#---- end ----

######################### Part II Recurrence Interval #########################

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
pb <- winProgressBar(title="Loop in progress", label="0% done", 
                     min=0, max=100, initial=0)
for (i in 1:22) {
  site.full <- readNWISdv(siteNumbers = site.nos[i], 
                      parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
    rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  site.latest <- site.full %>%
    mutate(Year = year(Date))%>%
    filter(Year > 1980)
  
  Recurrence.full <- site.full %>%
    mutate(Year = year(Date))%>%
    group_by(Year) %>%
    summarise(PeakDischarge = max(Discharge)) %>% 
    mutate(Rank = rank(-PeakDischarge),
           RecurrenceInterval = (length(Year) + 1)/Rank, 
           Probability = 1/RecurrenceInterval)
  
  Recurrence.latest <- site.latest %>%
    mutate(Year = year(Date))%>%
    group_by(Year) %>%
    summarise(PeakDischarge = max(Discharge)) %>% 
    mutate(Rank = rank(-PeakDischarge),
           RecurrenceInterval = (length(Year) + 1)/Rank, 
           Probability = 1/RecurrenceInterval)
  
  model.full <- lm(data = Recurrence.full, PeakDischarge ~ log(RecurrenceInterval))
  model.latest <- lm(data = Recurrence.latest, PeakDischarge ~ log(RecurrenceInterval))
  model.full.sum <- summary(model.full)
  model.latest.sum <- summary(model.latest)
  
  fig <- ggplot() +
    geom_point(data = Recurrence.full, aes(x = RecurrenceInterval, y = PeakDischarge),
               color = "lightskyblue4", size = 1, alpha = 0.7)+
    stat_smooth(data = Recurrence.full, aes(x = RecurrenceInterval, y = PeakDischarge),
                method = "lm" , formula = y ~ log(x),
                level = 0.95, alpha = 0.3, size = 0.75,
                color = "lightskyblue4", fill = "lightskyblue3") +
    geom_point(data = Recurrence.latest, aes(x = RecurrenceInterval, y = PeakDischarge),
               color = "darkseagreen4", size = 1, alpha = 0.7)+
    stat_smooth(data = Recurrence.latest, aes(x = RecurrenceInterval, y = PeakDischarge),
                method = "lm" , formula = y ~ log(x),
                level = 0.95, alpha = 0.4, size = 0.75,
                color = "darkseagreen4", fill = "darkseagreen3")+
    annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1, size = 3,
             label = paste("Site", site.list$site_lab[i], site.list$huc_cd[i], sep = " "))+
    labs(x = element_blank(), y = element_blank())
    
  assign(paste0("RecPlot", formatC(i, width = 2, flag = "0")), fig)
  assign(paste("mod", formatC(i, width = 2, flag = "0"), "full", sep = "."), model.full)
  assign(paste("mod", formatC(i, width = 2, flag = "0"), "latest", sep = "."), model.latest)
  assign(paste("sum", formatC(i, width = 2, flag = "0"), "full", sep = "."), model.full.sum)
  assign(paste("sum", formatC(i, width = 2, flag = "0"), "latest", sep = "."), model.latest.sum)
  rm(site.full, site.latest, Recurrence.full, Recurrence.latest,
     fig, model.full, model.latest, model.full.sum, model.latest.sum)
  
  # output info on progress
  prog <- sprintf("%d%% done", round((i/22)*100))
  setWinProgressBar(pb, i/(22)*100, label=prog)
}
close(pb)
##### Put results in a data frame
models.full <- mget(x = ls(pattern = "^mod.\\d\\d.full$"))
sums.full <- mget(x = ls(pattern = "^sum.\\d\\d.full$"))
models.latest <- mget(x = ls(pattern = "^mod.\\d\\d.latest$"))
sums.latest <- mget(x = ls(pattern = "^sum.\\d\\d.latest$"))

pred.peakdischarge <- site.list %>%
  select(site_lab, site_nm, huc4) %>%
  mutate(RecurrenceInterval = 50,
         full = NA, latest = NA,diff = NA)

for (i in 1:22) {
  pred.peakdischarge$full[[i]] <- 
    predict.lm(models.full[[i]], 
               data.frame(RecurrenceInterval = pred.peakdischarge$RecurrenceInterval[[i]]))
  pred.peakdischarge$latest[[i]] <- 
    predict.lm(models.latest[[i]], 
               data.frame(RecurrenceInterval = pred.peakdischarge$RecurrenceInterval[[i]]))
}

pred.peakdischarge <- pred.peakdischarge %>%
  mutate(diff = latest - full) %>%
  rename(post1980 = latest)
View(pred.peakdischarge)
#write.csv(pred.peakdischarge, file = "./Data/Processed/recurrence_int_50.csv", row.names = F)
pred.peakdischarge <- read.csv("./Data/Processed/recurrence_int_50.csv") %>%
  arrange(diff)

#---- loop end ----

##### Combine all Recurrence plots #####

# blue is full; green is latest
RecPlot <- ggarrange(RecPlot01, RecPlot02, RecPlot03, RecPlot04, RecPlot05, RecPlot06, 
                     RecPlot07, RecPlot08, RecPlot09, RecPlot10, RecPlot11, RecPlot12,
                     RecPlot13, RecPlot14, RecPlot15, RecPlot16, RecPlot17, RecPlot18,
                     RecPlot19, RecPlot20, RecPlot21, RecPlot22
                     + font("x.text", size = 10), ncol = 5, nrow = 5)
annotated.RecPlot <- 
  annotate_figure(RecPlot, bottom=text_grob("Recurrence Interval",color="black",hjust = 0.5,size = 13),
                  left = text_grob(expression("Discharge ft"^3*" s"^-1), color = "black",
                                   rot = 90, size = 13))

ggsave("./Figures/recurrence.png", annotated.RecPlot,
       dpi = 300, width = 14, height = 9, units = "in")
#---- Plot end ----

######################### Part III Variation over Years #########################

######## for loop ######## 
pb <- winProgressBar(title="Loop in progress", label="0% done", 
                     min=0, max=100, initial=0)
for (i in 1:22) {
  site <- readNWISdv(siteNumbers = site.nos[i], 
                          parameterCd = "00060", endDate = "2019-11-1")[,2:5] %>%
    rename(Discharge = X_00060_00003, Approval.Code = X_00060_00003_cd)
  
  std <- site %>%
    mutate(Year = year(Date))%>%
    group_by(Year) %>%
    summarise(SD = sd(Discharge))

  model <- lm(data = std, SD ~ Year)
  model.sum <- summary(model)
  
  fig <- ggplot() +
    geom_point(data = std, aes(x = Year, y = SD),
               color = "lightskyblue4", size = 1, alpha = 0.7)+
    stat_smooth(data = std, aes(x = Year, y = SD),
                method = "lm" , formula = y ~ x,
                geom = "line", size = 0.8,
                color = if_else(model$coefficients[[2]] > 0, "coral2", "cornflowerblue"),
                alpha = if_else(model.sum$coefficients[2,4] < 0.05, 1, 0.3)) +
    stat_smooth(data = std, aes(x = Year, y = SD),
                method = "lm" , formula = y ~ x, level = 0.95, 
                linetype = 0,
                color = if_else(model$coefficients[[2]] > 0, "coral2", "cornflowerblue"), 
                fill = if_else(model$coefficients[[2]] > 0, "coral2", "cornflowerblue"),
                alpha = if_else(model.sum$coefficients[2,4] < 0.05, 0.3, 0.1)) +
    annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1, size = 3,
             label = paste("Site", site.list$site_lab[i], site.list$huc_cd[i], sep = " "),
             fontface = if_else(model.sum$coefficients[2,4] < 0.05, "bold", "plain"))+
    labs(x = element_blank(), y = element_blank())
  
  assign(paste0("SdPlot", formatC(i, width = 2, flag = "0")), fig)
  assign(paste("mod", formatC(i, width = 2, flag = "0"), "sd", sep = "."), model)
  assign(paste("sum", formatC(i, width = 2, flag = "0"), "sd", sep = "."), model.sum)
  rm(site, std, model, model.sum, fig)
  
  # output info on progress
  prog <- sprintf("%d%% done", round((i/22)*100))
  setWinProgressBar(pb, i/(22)*100, label=prog)
}
close(pb)
##### Put results in data frame
models.sd <- mget(x = ls(pattern = "^mod.\\d\\d.sd$"))
sums.sd <- mget(x = ls(pattern = "^sum.\\d\\d.sd$"))

# Check assumptions
# for (i in 1:22) {
#   par(mfrow = c(2,2), mai = c(0.7,0.7,0.7,0.7))
#   plot(models.sd[[i]])
# }

sd.year <- site.list %>%
  select(site_lab, site_nm, huc4) %>%
  mutate(slope = NA, p = NA)

for (i in 1:22) {
  sd.year$slope[[i]] <- coef(models.sd[[i]])[[2]] %>%
    round(., digits = 3) %>%
    format(nsmall = 3)
  sd.year$p[[i]] <- sums.sd[[i]]$coefficients[2,4] %>%
    round(., digits = 4) %>%
    format(nsmall = 4)
}

# Adjust letter case; make state abbreviation constant
sd.year$site_nm <- gsub(sd.year$site_nm, pattern = "Nebr.", replacement = "NE")
sd.year <- sd.year %>%
  mutate(site_nm = paste0(str_to_title(str_extract(site_nm, pattern = ".*,")), 
                         str_extract(site_nm, pattern = "[:blank:][:upper:]{2}$")))
View(sd.year)

# write.csv(sd.year, file = "./Data/Processed/sd_year.csv", row.names = F)
sd.year <- read.csv("./Data/Processed/sd_year.csv")
sd.year <- arrange(sd.year, slope)

#---- loop end ----

##### Combine all SD vs year plots #####

SdPlot <- ggarrange(SdPlot01, SdPlot02, SdPlot03, SdPlot04, SdPlot05, SdPlot06, 
                    SdPlot07, SdPlot08, SdPlot09, SdPlot10, SdPlot11, SdPlot12,
                    SdPlot13, SdPlot14, SdPlot15, SdPlot16, SdPlot17, SdPlot18,
                    SdPlot19, SdPlot20, SdPlot21, SdPlot22
                    + font("x.text", size = 10), ncol = 5, nrow = 5)
annotated.SdPlot <- 
  annotate_figure(SdPlot, bottom=text_grob("Year",color = "black",hjust = 0.5,size = 13),
                  left = text_grob(expression("Standard Deviation within a Year"), color = "black",
                                   rot = 90, size = 13))

ggsave("./Figures/sd_year.png", annotated.SdPlot,
       dpi = 300, width = 14, height = 9, units = "in")
#---- Plot end ----

