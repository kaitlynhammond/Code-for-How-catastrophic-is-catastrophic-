library("terra")
library("sf")
library("tidyverse")


# Data setup --------------------------------------------------------------

#Load metric rasters
raw2007 <- rast("Spatial/mets2007_21_02_2024.grd")
raw2016 <- rast("Spatial/mets2016_21_02_2024.grd")


# Filter to keep coverage from both data sets -----------------------------

raw2007 <- mask(raw2007, raw2016)
raw2016 <- mask(raw2016, raw2007)



# Read rasters ------------------------------------------------------------
mtnash <- rast("Spatial/mtnash_extent.grd")
fire_rast_pre <- rast("Spatial/last_fire_pre2007.grd")
fire_rast_between <- rast("Spatial/last_fire_between.grd")
log_rast <- rast("Spatial/log_year.grd")
evc_rast <- rast("Spatial/EVC.grd")
firesev <- rast("Spatial/FIRE_SEV09.grd")



# Filter data -------------------------------------------------------------

#Keep only mountain ash
filt2007<- mask(raw2007, mtnash)
filt2016<- mask(raw2016, mtnash)


#Keep only forest >35m tall
tall_forest <- filt2007[["height99"]]
tall_forest[tall_forest <35] <- NA


filt2007 <- mask(filt2007, tall_forest)
filt2016 <- mask(filt2016, tall_forest)

#Remove anything that burned in between 2007 and 2015 but NOT 2009
filt2007 <- mask(filt2007, fire_rast_between, inverse = TRUE)
filt2016 <- mask(filt2016, fire_rast_between, inverse = TRUE)


#Remove recently burned
recent_fire <- fire_rast_pre
recent_fire[recent_fire < 1997] <- NA


filt2007 <- mask(filt2007, recent_fire, inverse = TRUE)
filt2016 <- mask(filt2016, recent_fire, inverse = TRUE)


#Remove rainforest and dry forest
rain_dry <- catalyze(evc_rast)
rain_dry[rain_dry == 1] <- 500
rain_dry[rain_dry == 10] <- 500
rain_dry[rain_dry <500] <- NA
plot(rain_dry)


filt2007 <- mask(filt2007, rain_dry, inverse = TRUE)
filt2016 <- mask(filt2016, rain_dry, inverse = TRUE)

#add the fire severity values
firesev_filt <- mask(firesev, filt2007[[1]])

filt2007 <- c(filt2007, firesev_filt)
filt2016 <- c(filt2016, firesev_filt)

raster::writeRaster(raster::brick(filt2007), "Data/filtered_2007mets_22_02_2024.grd")
raster::writeRaster(raster::brick(filt2016), "Data/filtered_2016mets_22_02_2024.grd")


# Calculate areas ---------------------------------------------------------

study_area <- filt2007[[1]]
study_area[!is.na(study_area)] <- 1
study_area <- freq(study_area)*400/10000
# 83815.36 ha

fire_area <- filt2007[[11]]
fire_area[!is.na(fire_area)] <- 1
fire_area <- freq(fire_area)*400/10000
#	31655.36 ha



# Convert to data frames and save -----------------------------------------

df2007 <- as.data.frame(filt2007, xy = TRUE)
df2007 <- df2007 %>%
  mutate(CLASS = ifelse(is.na(CLASS), 0, CLASS))
df2007 <- na.omit(df2007)


df2016 <- as.data.frame(filt2016, xy = TRUE)
df2016 <- df2016 %>%
  mutate(CLASS = ifelse(is.na(CLASS), 0, CLASS))
df2016 <- na.omit(df2016)



write.csv(df2007, "Data/filtered_2007mets_22_02_2024.csv", row.names = FALSE)
write.csv(df2016, "Data/filtered_2016mets_22_02_2024.csv", row.names = FALSE)




