library("e1071")
library("raster")
library("lidR")
library("rgdal")


#function for kurtosis
kfun <- function(Z){
  if (length(Z) < 4) {
    return(0)
  } else {
    kurtosis(Z, type = 2)
  }
}


#function for skewness
sfun <- function(Z){
  if (length(Z) < 3) {
    return(0)
  } else {
    skewness(Z, type = 2)
  }
}



#Function including all grid metrics
fun <- function (files) {
  file <- files
  file_name <- tools::file_path_sans_ext(file)
  file_name <- basename(file_name)  
  
  las <- readLAS(file, filter = "-keep_first -drop_z_below 1 -drop_z_above 90")
  crs(las) <- crs(fullr1)
  if (is.empty(las)) return(NULL)
  ext <- extent(las)
  r1 <- crop(fullr1, ext)
  r2 <- raster(extent(r1), res = 5)
  
  #Plot and sub plot density
  dens20 <- grid_metrics(las, ~length(Z), res = r1, filter = ~Z > 30)
  dens5 <- grid_metrics(las, ~length(Z), res = r2, filter = ~Z > 30)
  dens20[is.na(dens20[])] <- 0 
  dens5[is.na(dens5[])] <- 0 
  
  
  #Canopy heterogeneity (subplot variance/subplot mean)
  max_sub <- aggregate(dens5, fact = 4, fun = max, expand = TRUE)
  max_sub <- disaggregate(max_sub, fact = 4) #Scales by max number of hits
  scaled_sub <- dens5/max_sub
  mean_sub <- aggregate(scaled_sub, fact = 4, fun = mean)
  variance_sub <- aggregate(scaled_sub, fact = 4, fun = var)
  canopy_het <- variance_sub/mean_sub
  
  
  #Height metrics
  height50 <- grid_metrics(las, quantile(Z, probs = 0.50), res = r1)
  height95 <- grid_metrics(las, quantile(Z, probs = 0.95), res = r1)
  height99 <- grid_metrics(las, quantile(Z, probs = 0.99), res = r1)
  
  
  #Variability of heights
  heightsd <- grid_metrics(las, sd(Z), res = r1)
  canopysd <- grid_metrics(las, sd(Z), res = r1, filter = ~Z > 30)
  IQR <- grid_metrics(las, IQR(Z), res = r1)
  kurt <- grid_metrics(las, ~kfun(Z), res = r1)
  skew <- grid_metrics(las, ~sfun(Z), res = r1)
  
  
  #Cover
  dens_sub2 <- grid_metrics(las, ~length(Z), res = r2, filter = ~Z > 30)
  dens_sub2[dens_sub2 != 0] <- 1
  canopy_prop2 <- aggregate(dens_sub2, fact = 4, fun = sum)/16
  
  
  
  #Get the extent of the las and extend all the files
  rasts <- list(canopy_het, height50, height95, height99,
                heightsd, canopysd, IQR, kurt,
                skew, canopy_prop2)
  
  
  #Stack it all together
  rasts <- brick(rasts)
  names(rasts) <- c("canopy_het", "height50", "height95",
                    "height99", "sdheigt", "sdcanopy", 
                    "IQR", "kurt", "skew", "canopy_cover")
  
  return(rasts)
  
  #writeRaster(rasts, filename = paste0(output, "/", file_name, ".grd"))
}