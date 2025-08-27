.libPaths("/data/cephfs/punim0825/R_packages/")
library("raster")
library("lidR")
library("rgdal")

#Folder locations
partnumber <- "1"
RAWlidar <- paste0("/data/cephfs/punim0825/Lidar2016RAW/cep08-2015-16_central-highlands/cep08-2015-16_central-highlands_master-part", partnumber, "of6_Archived/irregular_points/ahd_las")
output <- "/scratch/punim0825/Normalized/2016/"
DTM <- raster(paste0("/scratch/punim0825/DEM_0323/part", partnumber, "/grid_metrics.tif"))
DTM

###################
####Normalizing
#Get the raw LiDAR files/select a portion of them to process
files <- list.files(path = paste0(RAWlidar), pattern = "*.las", full.names = TRUE,  recursive=FALSE)

#Create the output directory
dir.create(path = paste0(output, "part", partnumber), recursive = TRUE, showWarnings = FALSE)
normaldir <- paste0(output, "part", partnumber, "/")

#Creat the function to be applied
lasnorm = function(cluster, ...)
{
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  
  las <- lasnormalize(las, DTM, na.rm = TRUE)
  las <- lasfilter(las, buffer == 0)
  return(las)
}

#Set up catalog and options
ctg <- readLAScatalog(files)
opt_output_files(ctg) <- paste0(normaldir,"{XLEFT}_{YBOTTOM}")
ctg$processed <- FALSE
ctg$processed[1:100] <- TRUE


#Normalize the data
catalog_apply(ctg, lasnorm)