.libPaths("/data/cephfs/punim0825/R_packages/")
#Sys.setenv(R_LIBS = paste("/data/cephfs/punim0825/R_packages/", Sys.getenv("R_LIBS"), sep=.Platform$path.sep))
library("lidR")

#start time
Sys.time()

#Lidar location and output directories
partnumber <- "1" #1 of 6 folders raw data
date <- "DEM_0323"
dir.create(path = paste0("/scratch/punim0825/", date, "/part", partnumber), recursive = TRUE, showWarnings = FALSE)
output <- paste0("/scratch/punim0825/", date, "/part", partnumber, "/")
RAWlidar <- paste0("/data/cephfs/punim0825/Lidar2016RAW/cep08-2015-16_central-highlands/cep08-2015-16_central-highlands_master-part",
                 partnumber, "of6_Archived/irregular_points/ahd_las") #folder location

#Set up the catalog and give it properties
ctg <- readLAScatalog(RAWlidar)
opt_chunk_buffer(ctg) <- 30
opt_output_files(ctg) <- paste0(output, "{XLEFT}_{YBOTTOM}")
ctg$processed <- FALSE
ctg$processed[300:340] <- TRUE



#DTM calculation
lasdem = function(cluster, ...)
{
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  dem <- grid_terrain(las, res = .5, kriging(), keep_lowest = FALSE,
                      full_raster = FALSE)
  bbox <- raster::extent(cluster)
  dem <- raster::crop(dem, bbox)
 return(dem)
}

catalog_apply(ctg, lasdem)
Sys.time()
