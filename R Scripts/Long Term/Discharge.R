
########## Discharge Analysis ##########

######## Setup########

# Load Required Packages
pacman::p_load(tidyverse, dataRetrieval, lubridate, cowplot, grid, gridExtra, ggpubr)
theme_set(theme_classic())

# Import site info ####

site.list <- read_csv("./Data/Processed/bestsiteslists.csv", col_types = cols(
  X1 = "d", site_no = "c", site_nm = "c", huc_cd = "c", huc4 = "c", huc4_nm = "c"))
site.nos <- site.list$site_no
#----setup end ----

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
  assign(paste0("figure", site.list$huc4[2*i]), fig)
  
  rm(site1, site1.pattern, site2, site2.pattern, fig)
}


##### Combine all discharge plots #####

figure <- ggarrange(figure1020, figure1021, figure1022, figure1023, figure1024, figure1025, 
                    figure1026, figure1027, figure1028, figure1029, 
                    figure1030 + font("x.text", size = 10), ncol = 4, nrow = 3)
annotated.fig <- annotate_figure(figure,
                bottom = text_grob("Day of Year", color = "black", hjust = 0.5, size = 13),
                left = text_grob(expression("Discharge ft"^3*" s"^-1), 
                                 color = "black", rot = 90, size = 13))
ggsave("discharge.jpg", annotated.fig, dpi = 300, width = 14, height = 9, units = "in")

#---- end ----