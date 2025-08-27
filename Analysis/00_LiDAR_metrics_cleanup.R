library("terra")
library("sf")
library("tidyverse")


# Data setup --------------------------------------------------------------

#Load raw metric rasters
#Note: 2007 had different coverage so a different origin to 2016. Resampled to match.
raw2007 <- rast("Spatial/resample2007.grd")
raw2016 <- rast("Spatial/combined_2016mets.grd")

names(raw2007)  <- c("canopy_het", "height50", "height95",
                     "height99", "sdheigt", "sdcanopy", 
                     "IQR", "kurt", "skew", "canopy_cover")
names(raw2016) <- names(raw2007)


# Fix the canopy heterogeneity layer --------------------------------------
#Note: Tried to do on Spartan with weird edge effects, so fixed here

#2007
#Get the 5m resolution point density raster
canopy_2007 <- rast("Spatial/combined_2007bmets.grd")
raw2007 <- extend(raw2007, canopy_2007)
canopy_2007 <- extend(canopy_2007, raw2007[[1]])


#Calculate the variance/mean of point densities within a 20m space
max_sub <- aggregate(canopy_2007, fact = 4, fun = max)
max_sub <- mask(max_sub, raw2007[[1]])
max_sub <- disagg(max_sub, fact = 4) #Scales by max number of hits
scaled_sub <- canopy_2007/max_sub
mean_sub <- aggregate(scaled_sub, fact = 4, fun = mean)
variance_sub <- aggregate(scaled_sub, fact = 4, fun = var)
canopy_het <- variance_sub/mean_sub

#Add the layer to the other metrics
raw2007$canopy_het <- canopy_het

#Convert to a binary raster of areas with canopy over 30m 
#then sum and divide by 16 to get proportion cover
canopy_2007[canopy_2007 != 0] <- 1
canopy_cover <- aggregate(canopy_2007, fact = 4, fun = sum)/16

#Add to the raster
raw2007$canopy_cover <- canopy_cover

raster::writeRaster(raster::brick(raw2007), "Spatial/mets2007_21_02_2024.grd")


#2016
#Get the 5m resolution point density raster
canopy_2016 <- rast("Spatial/combined_2016bmets.grd")
raw2016 <- extend(raw2016, canopy_2016)
canopy_2016 <- extend(canopy_2016, raw2016[[1]])


#Calculate the variance/mean of point densities within a 20m space
max_sub <- aggregate(canopy_2016, fact = 4, fun = max)
max_sub <- mask(max_sub, raw2016[[1]])
max_sub <- disagg(max_sub, fact = 4) #Scales by max number of hits
scaled_sub <- canopy_2016/max_sub
mean_sub <- aggregate(scaled_sub, fact = 4, fun = mean)
variance_sub <- aggregate(scaled_sub, fact = 4, fun = var)
canopy_het <- variance_sub/mean_sub

#Add the layer to the other metrics
raw2016$canopy_het <- canopy_het

#Convert to a binary raster of areas with canopy over 30m 
#then sum and divide by 16 to get proportion cover
canopy_2016[canopy_2016 != 0] <- 1
canopy_cover <- aggregate(canopy_2016, fact = 4, fun = sum)/16

#Add to the raster
raw2016$canopy_cover <- canopy_cover

raster::writeRaster(raster::brick(raw2016), "Spatial/mets2016_21_02_2024.grd")