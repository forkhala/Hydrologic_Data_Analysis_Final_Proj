
########## Discharge Analysis ##########

######## Setup########

# Load Required Packages
pacman::p_load(tidyverse, dataRetrieval, lubridate)
theme_set(theme_classic())

# Import site nos
site.nos <- read_csv("./Data/Raw/bestsiteslists.csv", col_names = c("row.no", "site.no"), 
                     col_types = cols("row.no" = "i", "site.no" = "c"), skip = 1)[[2]]
#----setup end ----

######## HU 1023 Missouri-Little Sioux ########

##### 10230007 Platte River near Overton, Nebr. #####

# Import data from NWIS
hu10230007 <- readNWISdv(siteNumbers = site.nos[1], 
                          parameterCd = "00060", endDate = "2019-11-1")[,2:5]
names(hu10230007)[3:4] <- c("Discharge", "Approval.Code")

g10230007.1 <- ggplot(hu10230007, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "Year", y = expression("Discharge ft"^3*" s"^-1))

hu10230007.pattern <- hu10230007 %>%
  mutate(DOY = day(Date)) %>%
  group_by(DOY) %>%
  summarise(Median.Discharge = median(Discharge), 
            Max.Discharge = max(Discharge),
            Min.Discharge = min(Discharge))
g10230007.2 <- 
  ggplot(hu10230007.pattern, aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge)) +
  geom_line(aes(y = Max.Discharge), color = "gray") +
  geom_line(aes(y = Min.Discharge), color = "gray") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) 
print(g10230007.2)

#---- 10230007 ----

##### 10200202 Lower Platte #####

hu10200202 <- readNWISdv(siteNumbers = site.nos[1], 
                         parameterCd = "00060", endDate = "2019-11-1")[,2:5]
names(hu10200202)[3:4] <- c("Discharge", "Approval.Code")

g10200202.1 <- ggplot(hu10200202, aes(x = Date, y = Discharge)) +
  geom_line() +
  labs(x = "Year", y = expression("Discharge ft"^3*" s"^-1))

hu10200202.pattern <- hu10200202 %>%
  mutate(DOY = day(Date)) %>%
  group_by(DOY) %>%
  summarise(Median.Discharge = median(Discharge), 
            Max.Discharge = max(Discharge),
            Min.Discharge = min(Discharge))
g10200202.2 <- 
  ggplot(hu10230007.pattern, aes(x = DOY)) +
  geom_line(aes(y = Median.Discharge)) +
  geom_line(aes(y = Max.Discharge), color = "gray") +
  geom_line(aes(y = Min.Discharge), color = "gray") +  
  labs(x = "Day of Year", y = expression("Discharge ft"^3*" s"^-1)) 
print(g10200202.2)




