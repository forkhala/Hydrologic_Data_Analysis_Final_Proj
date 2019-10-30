library(dataRetrieval)
library(tidyverse)

allSites <- whatNWISdata(huc="10290203")

huc1029 <- seq(10290101,10290110,1)
huc1021 <- seq(10210001,10210010,1)

huc1029sites <- whatNWISdata(huc = huc1029)

huc1021sites <- whatNWISdata(huc = huc1021)
huc1021sites <- huc1021sites %>%
  filter(parm_cd %in% c("00600", "00060", "00665"))

huc1021Discharge <- huc1021sites %>%
  filter(parm_cd=="00060")

huc1021Nitrogen <- huc1021sites %>%
  filter(parm_cd=="00600")

huc1021join <- left_join(huc1021Discharge,huc1021Nitrogen, by = "site_no")
