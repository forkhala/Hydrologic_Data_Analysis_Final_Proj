#best sites

bestsites1021.1026.1027.1030 <- 
  c("06775900", "06794000", "06877600", "06874000", "06892350", 
    "06887500", "06934500", "06894000")
bestsites1024.1025 <- c("06844500", "06856600", "06818000", "06810000")
bestsites1020.1023 <- c("06768000", "06805500", "06775900", "06794000", 
                        "06800000", "06800500", "06609500", "06610000")
bestsites1028.1029 <- c("06902000", "06905500", "06921070", "06926510")

best.sites <- c(bestsites1021.1026.1027.1030, bestsites1024.1025, 
                bestsites1020.1023, bestsites1028.1029)
best.sites <- unique(best.sites)


SiteNo <- best.sites[22]

SiteNo.dv.dis <- readNWISdv(SiteNo, "00060")
SiteNo.dis <- SiteNo.dv.dis %>% 
  dplyr::select(Site = site_no, Date = Date, Discharge = X_00060_00003)
SiteNo.qw.n <- readNWISqw(SiteNo, "00600")
SiteNo.n <- SiteNo.qw.n %>%
  dplyr::select(Site = site_no, Date = sample_dt, Nitrogen = result_va)
SiteNo.dis.n.join <- left_join(SiteNo.dis, SiteNo.n, by = c("Site", "Date"))
ggplot(SiteNo.dis.n.join, aes(x=Discharge, y= log(Nitrogen))) +
  geom_point() +
  geom_smooth(method=lm)
mod1 <- lm(data = SiteNo.dis.n.join, log(Nitrogen) ~ Discharge)
summary(mod1)

SiteNo <- best.sites[1]

SiteNo.peak <- readNWISpeak(SiteNo)
ggplot(SiteNo.peak, aes(x=peak_dt, y=peak_va)) + geom_point()
mod <- lm(data= SiteNo.peak, peak_va ~ peak_dt)
summary(mod)


