########## Generate Map of Selected Sites ##########

#### Setup###
pacman::p_load(tidyverse, dataRetrieval, sf, maps, foreign, RColorBrewer)
# devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
theme_set(theme_classic())

##### Map sites selected, water bodies, and watersheds in the study region ####
# Gathering site nos from scripts
bestsites1021.1026.1027.1030 <- 
  c("06775900", "06794000", "06877600", "06874000", "06892350", 
    "06887500", "06934500", "06894000")
bestsites1024.1025 <- c("06844500", "06856600", "06818000", "06810000")
bestsites1020.1023 <- c("06768000", "06805500", "06775900", "06794000", 
                        "06800000", "06800500", "06609500", "06610000")
bestsites1028.1029 <- c("06902000", "06905500", "06921070", "06926510")

best.sites <- c(bestsites1020.1023, bestsites1021.1026.1027.1030, 
                bestsites1024.1025, bestsites1028.1029)
best.sites <- unique(best.sites)
# Generate a list of selected sites with info on huc and names
site.list <- whatNWISdata(siteNumbers = best.sites, parameterCd = "00060") %>%
  select(site_no, station_nm, huc_cd) %>%
  group_by(site_no) %>%
  summarise(site_nm = first(station_nm),
            huc_cd = first(huc_cd)) %>%
  arrange(huc_cd) %>%
  mutate(huc4 = substr(huc_cd, start = 1, stop = 4),
         site_lab = seq(1, 22, by = 1))

huc4_nm <- rep(c("Platte", "Loup", "Elkhorn", "Missouri-Little Sioux", "Missouri-Nishnabotna",
                 "Republican", "Smoky Hill", "Kansas", "Chariton-Grand", "Gasconade-Osage",
                 "Lower Missouri"), each = 2)
site.list <- cbind(site.list, huc4_nm)
# save the list
# write.csv(site.list, file="./Data/Processed/bestsiteslist.csv")
# ---- site selection end ----

########## Mapping Best Sites ##########

#### Basic layers ####
best.sites.info <- whatNWISdata(sites=best.sites)

best.sites.lat.long <- best.sites.info %>%
  group_by(site_no) %>%
  summarise(Lat = mean(dec_lat_va), Long=mean(dec_long_va)) %>%
  left_join(., site.list, by = "site_no")

#creating spatial dataframe and map
best.sites.spatial <- st_as_sf(best.sites.lat.long,
                                   coords=c("Long", "Lat"), crs = 4269)
proj <- st_crs(best.sites.spatial) #lat and long are in 4269

# all states related to missouri region
states <- st_as_sf(map(database = "state", plot = TRUE, fill = TRUE, col = "white"))
allstates.map <- states %>% 
  filter(ID %in% c("montana","north dakota","south dakota","nebraska","iowa","kansas","missouri",
                   "wyoming","colorado", "minnesota", "idaho"))
allstates.map <- st_set_crs(allstates.map, proj) #set projection
#---- basic layers end----

###### Using Watershed (HUC4) Boundary #####
# Still need run most codes above, except waterfeature ones (starting around line 44)
# and those producing graphs

# import watershed shapefile; DO NOT open this shapefile in R
AllHUC4 <- st_read("./Data/Shapefiles/WBD_10_HU2_Shape/WBDHU4.dbf")
HUC4.SE <- AllHUC4 %>%
  filter(HUC4 %in% seq(from = 1020, to = 1030, by = 1)) %>%
  mutate(Name = paste(HUC4, Name, sep = " "))
HUC4.NW <- AllHUC4 %>%
  filter(HUC4 %in% seq(from = 1000, to = 1019, by = 1))

# import stream geometric file (NOTE shapefiles not included in repos)
st_layers("../Untracked Proj Data/Small_Scale_Map/hydr48m010g.gdb")
streams <- st_read("../Untracked Proj Data/Small_Scale_Map/hydr48m010g.gdb", layer = "Stream")%>%
  st_zm(drop = T, what = "ZM") # drop z/m dimension, and only keep x, y dimensions for 2D figures
st_crs(streams)

streams.HU10 <- streams %>%
  filter(Region == 10 & Strahler > 2)

missouri <- streams.HU10 %>%
  filter(Name == "Missouri River")

sitemap <- ggplot() +
  geom_sf(data = allstates.map, fill = "white", size = 0.4) +
  geom_sf(data = HUC4.SE, aes(fill = Name), alpha = 0.3, size = 0.45) +
  geom_sf(data = HUC4.NW, fill = "grey85", alpha = 0.1, size = 0.45) +
  geom_sf(data = streams.HU10, color = "lightskyblue2", alpha = 0.8, size = 0.4) +
  geom_sf(data = missouri, color = "dodgerblue2", alpha = 0.75, size = 0.7) +
  scale_fill_brewer(palette = "Paired") +
  geom_sf(data = best.sites.spatial, fill="black", color="black", 
          alpha = 0.7, size = 1.15) +
  theme(legend.margin = margin(0,0,0,0, "pt"), legend.text = element_text(size = 7.5), 
        legend.title = element_text(size = 8.5), plot.margin=unit(c(0.2,0.2,0.2,0.2),"in")) + 
  labs(fill = "Watershed Name",x = element_blank(), y = element_blank())+
  geom_sf_text_repel(data = best.sites.spatial, aes(label = site_lab), 
                     force = 1.5, box.padding = 0.30, min.segment.length = 0.4)

# Caution this takes time to display, and even longer than ggsave()
# print(sitemap)
# save file
ggsave("./Figures/site_map.jpg", sitemap, dpi = 300, width = 9, height = 5.3, units = "in")

#---- site mapping ends ----

########## Mapping Impaired Water (cause: nutrient) ##########

# Import shapefile for all impaired rivers
impaired <- st_read("../Untracked Proj Data/303d_201505/rad_303d_20150501/rad_303d_a.dbf")
# head(impaired)
impaired.hu10 <- impaired %>% 
  filter(str_detect(HUC12, "10..........")) %>%
  drop_na(HUC12)

# Import data frame of impaired rivers and the causes; filter for nutrient impairment
impaired.cause <- read.dbf("../Untracked Proj Data/303d_201505/rad_303d_20150501/attgeo_303dcaussrce.dbf")
head(impaired.cause)

impaired.nutrient <- impaired.cause %>%
  filter(str_detect(LW_PARC_NM, "NUTRIENT"))%>%
  mutate(LW_PARC_NM = if_else(LW_PARC_NM == "NUTRIENTS", "Impaired Waters", as.character(LW_PARC_NM)))
# unique(impaired.nutrient$LW_DETC_NM)

# Generate sf object for rivers imparied by nutrient
impaired.map <- inner_join(impaired.hu10, impaired.nutrient)

# Plot map w/ sites and impaired waters
impairedplot <- ggplot() +
  geom_sf(data = allstates.map, fill = "white", size = 0.4) +
  geom_sf(data = HUC4.SE, aes(fill = Name), alpha = 0.3, size = 0.45) +
  geom_sf(data = HUC4.NW, fill = "grey85", alpha = 0.1, size = 0.45) +
  geom_sf(data = streams.HU10, color = "lightskyblue2", alpha = 0.8, size = 0.4) +
  geom_sf(data = missouri, color = "dodgerblue2", alpha = 0.75, size = 0.7) +
  geom_sf(data = impaired.map, color = "Red")+
  scale_fill_brewer(palette = "Paired") +
  geom_sf(data = best.sites.spatial, fill="black", color="black", 
          alpha = 0.7, size = 1.15) +
  theme(legend.margin = margin(0,0,0,0, "pt"), legend.text = element_text(size = 7.5), 
        legend.title = element_text(size = 8.5), plot.margin=unit(c(0.2,0.2,0.2,0.2),"in")) + 
  labs(fill = "Watershed Name",x = element_blank(), y = element_blank()) +
  geom_sf_text_repel(data = best.sites.spatial, aes(label = site_lab), 
                     force = 1.5, box.padding = 0.30, min.segment.length = 0.4)

ggsave("./Figures/impaired.jpg", impairedplot, dpi = 300, width = 9, height = 5.3, units = "in")
#---- Impaired map end ----

########## Agricultural Use Map ##########

# import geotiff file using raster
require(raster)
require(gdalUtilities)
#landcover.raster <- raster("../Untracked Proj Data/Land_Cover/lc_13/gaplf2011lc_v30_lcc_13.tif")
landcover.raster.co <- raster("../Untracked Proj Data/Land_Cover/lc_CO/gaplf2011lc_v30_co.tif")
landcover.raster.ia <- raster("../Untracked Proj Data/Land_Cover/lc_IA/gaplf2011lc_v30_ia.tif")
landcover.raster.ks <- raster("../Untracked Proj Data/Land_Cover/lc_KS/gaplf2011lc_v30_ks.tif")
landcover.raster.mn <- raster("../Untracked Proj Data/Land_Cover/lc_MN/gaplf2011lc_v30_mn.tif")
landcover.raster.mo <- raster("../Untracked Proj Data/Land_Cover/lc_MO/gaplf2011lc_v30_mo.tif")
landcover.raster.mt <- raster("../Untracked Proj Data/Land_Cover/lc_MT/gaplf2011lc_v30_mt.tif")
landcover.raster.nd <- raster("../Untracked Proj Data/Land_Cover/lc_ND/gaplf2011lc_v30_nd.tif")
landcover.raster.ne <- raster("../Untracked Proj Data/Land_Cover/lc_NE/gaplf2011lc_v30_ne.tif")
landcover.raster.sd <- raster("../Untracked Proj Data/Land_Cover/lc_SD/gaplf2011lc_v30_sd.tif")
landcover.raster.wy <- raster("../Untracked Proj Data/Land_Cover/lc_WY/gaplf2011lc_v30_wy.tif")
landcover.raster.list <- list(landcover.raster.co,landcover.raster.ia, landcover.raster.ks, 
                              landcover.raster.mn,landcover.raster.mo, landcover.raster.mt, 
                              landcover.raster.nd,landcover.raster.ne, landcover.raster.sd, 
                              landcover.raster.wy)
raster.list.nm <- c("co", "ia", "ks", "mn", "mo", "mt", "nd", "ne", "sd","wy")

# Check basic info and fiel structure
structure(landcover.raster.ia)
levels(landcover.raster.ia)

### Select pixels for agricultural lands
# 556: Herbaceous Agricultural Vegetation	-	Row & Close Grain Crop Cultural Formation
# 557: Herbaceous Agricultural Vegetation	-	Pasture & Hay Field Crop
crop.raster.co <- landcover.raster.co == 556
structure(crop.raster.co)
plot(crop.raster)

# Reduce resolution; original 30m x 30m too detailed; change to 300m x 300m
writeRaster(crop.raster, f <- tempfile(fileext='.tif'), datatype='INT1U')
system.time(gdalUtilities::gdalwarp(f, "../Untracked Proj Data/temp.tif",
                                    r='near', multi=TRUE, tr=res(crop.raster)*100))
crop.raster.low <- raster("../Untracked Proj Data/temp.tif")
plot(crop.raster.low)

# Set projection and change raster extent to longitude and latitude
(crs <- crs(allstates.map))
crop.raster.map.co <- projectRaster(crop.raster.low.co, crs = crs)
structure(crop.raster.map.co)

##### THIS SECTION NOT WORKING #####
# Use loop to perform most of the procedure above for all states 
# **CAUTION DO NOT RUN THE LOOP ON A LAPTOP**

# Generate progress bar for loop 
pb <- winProgressBar(title="Extracting pixels of agricultural lands", label="0% done", 
                     min=0, max=100, initial=0)
for (i in 1:10) {
  crop.raster.temp <- landcover.raster.list[[i]] == 556
  # crop.raster.low.temp <- aggregate(crop.raster.temp, fact = 10)
  
  # Use gdalUtilities::gdalwarp; much faster than raster function aggregate())
  writeRaster(crop.raster.temp, f <- tempfile(fileext='.tif'), datatype='INT1U')
  system.time(gdalUtilities::gdalwarp(f, "../Untracked Proj Data/temp.tif",
                                      r='mode', multi=TRUE, tr=res(crop.raster)*100))
  crop.raster.low.temp <- raster("../Untracked Proj Data/temp.tif")
  
  (crs <- crs(allstates.map))
  # crop.raster.map.temp <- projectRaster(crop.raster.low.temp, crs = crs, method = "ngb")
  # projection method for categorical variables
  
  assign(paste0("crop.raster.", raster.list.nm[[i]]), crop.raster.temp)
  assign(paste0("crop.raster.low.", raster.list.nm[[i]]), crop.raster.low.temp)
  # assign(paste0("crop.raster.map.", raster.list.nm[[i]]), crop.raster.map.temp)
  
  rm(crop.raster.temp, crop.raster.low.temp, crop.raster.map.temp)
  
  # output info on progress
  prog <- sprintf("%d%% done", round((i/10)*100))
  setWinProgressBar(pb, i/(10)*100, label=prog)
}
close(pb)

# Merge raster objects for all states
structure(crop.raster.map.co); structure(crop.raster.map.ia);structure(crop.raster.map.ks)
structure(crop.raster.map.mn); structure(crop.raster.map.mo);structure(crop.raster.map.mt)
structure(crop.raster.map.nd);structure(crop.raster.map.ne);structure(crop.raster.map.sd)
structure(crop.raster.map.wy)
origin(crop.raster.map.co); origin(crop.raster.map.ia);origin(crop.raster.map.ks)
origin(crop.raster.map.mn); origin(crop.raster.map.mo);origin(crop.raster.map.mt)
origin(crop.raster.map.nd);origin(crop.raster.map.ne);origin(crop.raster.map.sd)
origin(crop.raster.map.wy)

crop.raster.map.allstates <- raster::merge(crop.raster.map.co,crop.raster.map.ia,crop.raster.map.ks,
                                       crop.raster.map.mn,crop.raster.map.mo,crop.raster.map.mt,
                                       crop.raster.map.nd,crop.raster.map.ne,crop.raster.map.sd,
                                       crop.raster.map.wy, tolerance = 0.1)


structure(crop.raster.low.co); structure(crop.raster.low.ia);structure(crop.raster.low.ks)
structure(crop.raster.low.mn); structure(crop.raster.low.mo);structure(crop.raster.low.mt)
structure(crop.raster.low.nd);structure(crop.raster.low.ne);structure(crop.raster.low.sd)
structure(crop.raster.low.wy)

crop.raster.low.allstates <- raster::merge(crop.raster.low.co,crop.raster.low.ia,crop.raster.low.ks,
                                           crop.raster.low.mn,crop.raster.low.mo,crop.raster.low.mt,
                                           crop.raster.low.nd,crop.raster.low.ne,crop.raster.low.sd,
                                           crop.raster.low.wy)
for (i in 10) {
  writeRaster(crop.raster.map.allstates[[i]],
              filename = paste("../Untracked Proj Data/Land_Cover/cropmap.", raster.list.nm[[i]]),
              format = "raster")
}
#---- END ----

##### Merge before processing it (origin issue otherwise)  #####
pb <- winProgressBar(title="Extracting pixels of agricultural lands", label="0% done", 
                     min=0, max=100, initial=0)
for (i in 1:10) {
  crop.raster.temp <- landcover.raster.list[[i]] == 556
  assign(paste0("crop.raster.", raster.list.nm[[i]]), crop.raster.temp)
  rm(crop.raster.temp)
  
  # output info on progress
  prog <- sprintf("%d%% done", round((i/10)*100))
  setWinProgressBar(pb, i/(10)*100, label=prog)
}
close(pb)
# Merging is **COMPUTATIONALLY INTENSIVE & SLOW**
crop.raster.allstates <- 
  raster::merge(crop.raster.co,crop.raster.ia,crop.raster.ks,crop.raster.mn,
                crop.raster.mo,crop.raster.mt,crop.raster.nd,crop.raster.ne,
                crop.raster.sd,crop.raster.wy, overlap = TRUE,
                filename = "../Untracked Proj Data/Land_Cover/cropmap_allstates_high.tif")
plot(crop.raster.allstates)

# Reduce resolution

# temp <- raster::aggregate(crop.raster.allstates, fact = 10, dissolve = T)
writeRaster(crop.raster.allstates, f <- tempfile(fileext='.tif'), datatype='INT1U')
gdalUtilities::gdalwarp(f, "../Untracked Proj Data/Land_Cover/cropmap_allstates_low.tif",
                        r='mode', multi=TRUE, tr=res(crop.raster)*100)

crop.raster.low.allstates <- raster("../Untracked Proj Data/Land_Cover/cropmap_allstates_low.tif")
plot(crop.raster.low.allstates)

# Project & adjust extent
(crs <- crs(allstates.map))
crop.raster.map.allstates <- projectRaster(crop.raster.low.allstates, 
                                           crs = crs, method = "ngb")
structure(crop.raster.map.allstates)

### Convert raster object to polygon/shapefiles NOTE MAY TAKE SOME TIME
# pol <- rasterToPolygons(crop.raster.map, dissolve = T)

# Using the fantastic function written by John Baumgartner
# https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/ 
# https://gist.github.com/johnbaums/26e8091f082f2b3dd279 

Sys.which("gdal_polygonize.py")

## Define the function
polygonizer <- function(x, outshape=NULL, pypath=NULL, readpoly=TRUE, 
                        fillholes=FALSE, aggregate=FALSE, 
                        quietish=TRUE) {
  # x: an R Raster layer, or the file path to a raster file recognised by GDAL 
  # outshape: the path to the output shapefile (if NULL, a temporary file will 
  #           be created) 
  # pypath: the path to gdal_polygonize.py or OSGeo4W.bat (if NULL, the function 
  #         will attempt to determine the location)
  # readpoly: should the polygon shapefile be read back into R, and returned by
  #           this function? (logical) 
  # fillholes: should holes be deleted (i.e., their area added to the containing
  #            polygon)
  # aggregate: should polygons be aggregated by their associated raster value?
  # quietish: should (some) messages be suppressed? (logical)
  if (isTRUE(readpoly) || isTRUE(fillholes)) require(rgdal)
  if (isTRUE(aggregate)) require(rgeos)
  if (is.null(pypath)) {
    cmd <- Sys.which('OSGeo4W.bat')
    pypath <- 'gdal_polygonize'
    if(cmd=='') {
      cmd <- 'python'
      pypath <- Sys.which('gdal_polygonize.py')
      if (!file.exists(pypath)) 
        stop("Could not find gdal_polygonize.py or OSGeo4W on your system.") 
    }
  }
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists)) 
      stop(sprintf('File already exists: %s', 
                   toString(paste(outshape, c('shp', 'shx', 'dbf'), 
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  
  system2(cmd, args=(
    sprintf('"%s" "%s" %s -f "ESRI Shapefile" "%s.shp"', 
            pypath, rastpath, ifelse(quietish, '-q ', ''), outshape)))
  
  if(isTRUE(aggregate)||isTRUE(readpoly)||isTRUE(fillholes)) {
    shp <- readOGR(dirname(outshape), layer=basename(outshape), 
                   verbose=!quietish)    
  } else return(NULL)
  
  if (isTRUE(fillholes)) {
    poly_noholes <- lapply(shp@polygons, function(x) {
      Filter(function(p) p@ringDir==1, x@Polygons)[[1]]
    })
    pp <- SpatialPolygons(mapply(function(x, id) {
      list(Polygons(list(x), ID=id))
    }, poly_noholes, row.names(shp)), proj4string=CRS(proj4string(shp)))
    shp <- SpatialPolygonsDataFrame(pp, shp@data)
    if(isTRUE(aggregate)) shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  if(isTRUE(aggregate) & !isTRUE(fillholes)) {
    shp <- aggregate(shp, names(shp))
    writeOGR(shp, dirname(outshape), basename(outshape), 
             'ESRI Shapefile', overwrite=TRUE)
  }
  ifelse(isTRUE(readpoly), return(shp), return(NULL))
}

# # Testing ####
# library(rasterVis)
# download.file('https://www.dropbox.com/s/tk3kg2oce4h2snd/NEcountries.asc.zip?dl=1',
#               destfile={f <- tempfile()}, quiet=TRUE, cacheOK=FALSE,
#               mode='wb')
# unzip(f, exdir = d <- tempdir() )
# r <- raster(file.path(d, 'NEcountries.asc'), crs='+proj=longlat')
# p <- polygonizer(r)
# spplot(p, col.regions=rainbow(200))
# #---testing end----

system.time(polygonizer(crop.raster.map.allstates, aggregate = T, readpoly = F,
                        outshape = "./Data/Shapefiles/Land_Cover/cropland"))

crop.sf.allstates <- st_read("../Untracked Proj Data/Land_Cover/cropland.shp")
st_crs(crop.sf.allstates)

cropland.map <- crop.sf.allstates %>%
  filter(DN == 1) %>%
  mutate(land = if_else(DN == 1, "Cropland", "NA")) %>%
  select(-DN)

### Manually set colors
colornames <- c(brewer.pal(11, "Paired"), "red", "burlywood3")
variables <- c(HUC4.SE$Name, "Impaired Waters", "Cropland")
names(colornames) <- variables

cropplot1 <- ggplot() +
  geom_sf(data = allstates.map, fill = "white", size = 0.4) +
  geom_sf(data = cropland.map, aes(fill = land), alpha=0.4, linetype = 0) +
  geom_sf(data = HUC4.SE, aes(fill = Name), alpha = 0.3, size = 0.45) +
  geom_sf(data = HUC4.NW, fill = "grey85", alpha = 0.1, size = 0.45) +
  geom_sf(data = streams.HU10, color = "lightskyblue2", alpha = 0.8, size = 0.4) +
  geom_sf(data = impaired.map, aes(fill = LW_PARC_NM), color = "red", alpha = 0.8)+
  geom_sf(data = best.sites.spatial, fill="black", color="black", 
          alpha = 0.8, size = 1.15) +
  scale_fill_manual(values = colornames, name = "Watershed Names and \n Landscape Features") +
  geom_sf_text_repel(data = best.sites.spatial, aes(label = site_lab),
                     force = 1.5, box.padding = 0.30, min.segment.length = 0.4) +
  theme(legend.margin = margin(0,0,0,0, "pt"), legend.text = element_text(size = 7.5),
        legend.title = element_text(size = 8.5), plot.margin=unit(c(0.2,0.2,0.2,0.2),"in")) +
  labs(x = element_blank(), y = element_blank())+
  guides(fill = guide_legend(override.aes = list(alpha = 0.2, color = NA)))


ggsave("./Figures/cropland1.jpg", cropplot1, dpi = 300, width = 9, height = 5.3, units = "in")

cropplot2 <- ggplot() +
  geom_sf(data = allstates.map, fill = "white", size = 0.4) +
  geom_sf(data = cropland.map, aes(fill = land), alpha=0.4, linetype = 0) +
  geom_sf(data = HUC4.SE, fill = "gray85", alpha = 0.1, size = 0.45) +
  geom_sf(data = HUC4.NW, fill = "grey85", alpha = 0.1, size = 0.45) +
  geom_sf(data = streams.HU10, color = "lightskyblue2", alpha = 0.8, size = 0.4) +
  geom_sf(data = impaired.map, aes(fill = LW_PARC_NM), color = "red", alpha = 0.8)+
  geom_sf(data = best.sites.spatial, fill="black", color="black", 
          alpha = 0.8, size = 1.15) +
  scale_fill_manual(values = colornames[12:13], name = "") +
  geom_sf_text_repel(data = best.sites.spatial, aes(label = site_lab),
                     force = 1.5, box.padding = 0.30, min.segment.length = 0.4) +
  theme(legend.margin = margin(0,0,0,0, "pt"), legend.text = element_text(size = 7.5),
        legend.title = element_text(size = 8.5), plot.margin=unit(c(0.2,0.2,0.2,0.2),"in")) +
  labs(x = element_blank(), y = element_blank())+
  guides(fill = guide_legend(override.aes = list(alpha = 0.3, color = NA)))


ggsave("./Figures/cropland2.jpg", cropplot2, dpi = 300, width = 9, height = 5.3, units = "in")

