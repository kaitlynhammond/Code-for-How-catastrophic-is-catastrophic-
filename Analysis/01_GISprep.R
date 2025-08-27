library("terra")
library("sf")
library("tidyverse")

# Data setup --------------------------------------------------------------

#Load metric rasters (generated on Spartan)
raw2007 <- rast("Spatial/mets2007_21_02_2024.grd")
raw2016 <- rast("Spatial/mets2016_21_02_2024.grd")


# Generate plot history info ----------------------------------------------

#Create a blank template raster
blank_rast <- rast(ext = ext(raw2016), res = res(raw2016), crs = crs(raw2016))


#Load relevant shapefiles
fire_hist <- vect("Spatial/FIRE_HISTORY.shp")
fire_hist <- project(fire_hist, crs(blank_rast))
fire_hist <- crop(fire_hist, blank_rast)
fire_pre <- st_as_sf(fire_hist) %>%
  select(SEASON, geometry) %>%
  filter(SEASON < 2007)
fire_between <- st_as_sf(fire_hist) %>%
  select(SEASON, geometry) %>%
  filter(SEASON > 2007 & SEASON < 2016 & SEASON != 2009)

log_hist <- vect("Spatial/LASTLOG25.shp")
log_hist <- project(log_hist, crs(blank_rast))
log_hist <- crop(log_hist, blank_rast)
log_hist <- st_as_sf(log_hist) %>%
  select(SEASON, geometry) %>%
  mutate(year = as.numeric(substr(SEASON, 1 , 4))+1) %>%
  filter(year <2009)
log_hist <- vect(log_hist)


#evc_layer <- read_sf("Spatial/NV2005_EVCBCS.shp")
#evc_crop <- st_transform(evc_layer, crs(blank_rast))
#evc_crop <- st_crop(evc_crop, st_bbox(blank_rast))
#st_write(evc_crop, "Spatial", layer = "EVC_crop", driver = "ESRI Shapefile") 
    #Note: Cropped because it's enormous
evc_crop <- vect("Spatial/EVC_crop.shp")
evc_crop <- project(evc_crop, crs(blank_rast))
evc_subset <- st_as_sf(evc_crop) %>%
  select(EVC, X_EVCNAME, X_GROUPNAM, geometry)

evc_subset <- vect(evc_subset)



# Rasterize the shapefiles ------------------------------------------------

fire_rast_pre <- rasterize(fire_pre, blank_rast, field = "SEASON", fun = max)
raster::writeRaster(raster::raster(fire_rast_pre), "Spatial/last_fire_pre2007.grd")

fire_rast_between <- rasterize(fire_between, blank_rast, field = "SEASON", fun = max)
raster::writeRaster(raster::raster(fire_rast_between), "Spatial/last_fire_between.grd")

log_rast <- rasterize(log_hist, blank_rast, field = "year", fun = max)
raster::writeRaster(raster::raster(log_rast), "Spatial/log_year.grd")

evc_rast <- rasterize(evc_subset, blank_rast, field = "X_GROUPNAM")
raster::writeRaster(raster::raster(evc_rast), "Spatial/EVC.grd")



# Create the mtn ash and fire sev rasters ---------------------------------

landis_template <- rast("Spatial/IC_2019_V6.img")
eregs_T0 <- rast("Spatial/eucaregn-MAX-0.img")
ext(eregs_T0) <- ext(landis_template)
crs(eregs_T0) <- crs(landis_template)


mtnash <- project(eregs_T0, crs(blank_rast))
mtnash[mtnash == 0] <- NA
mtnash[!is.na(mtnash)] <- 1
mtnash <- resample(mtnash, blank_rast)

raster::writeRaster(raster::raster(mtnash), "Spatial/mtnash_extent.grd", overwrite = TRUE)


sev09 <- vect("Spatial/FIRE_SEV09_POLY.shp")
sev09 <- project(sev09 , crs(blank_rast))
sev09 <- crop(sev09 , blank_rast)
sev09 <- st_as_sf(sev09) %>%
  select(CLASS, geometry)

sev09_rast <- rasterize(sev09, blank_rast, field = "CLASS")
raster::writeRaster(raster::raster(sev09_rast), "Spatial/FIRE_SEV09.grd")
