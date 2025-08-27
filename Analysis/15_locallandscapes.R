library("sf")
library("terra")
library("landscapemetrics")
library("tidyverse")


buffer1000 <- st_read("Data/buffer1000.shp")
buffer1000 <- buffer1000 %>%
  select(geometry)


rast2007 <- rast("Outputs/predicted2007.grd")


all_rasters <- list()

for(i in 1:nrow(buffer1000)){
  
  local <- buffer1000[i,]
  landscape <- crop(rast2007, local)
  landscape[is.na(landscape)] <- 0
  
  all_rasters[[i]] <- landscape
}




# Landscape metrics local landscapes --------------------------------------
lsm_list <- list()

for(i in 1:nrow(buffer1000)){
  
  current_rast <- all_rasters[[i]]
  current_rast[current_rast > 0] <- 1

  lsm <- calculate_lsm(current_rast, #input raster
                     what = c("lsm_c_ca", #class area
                              "lsm_c_np",#number of patches
                              "lsm_c_area_mn",#Mean patch area
                              "lsm_c_area_sd", #SD of patch area
                              "lsm_l_ed",# Edge density
                              "lsm_l_contag",#contagion
                              "lsm_l_lpi", #largest patch index
                              "lsm_l_enn_mn"), #mean euclidean nearest neighbour), 
                     directions = 8) #This is the neighborhood rule we are using

lsm$row <- i
lsm_list[[i]] <- lsm


}


lsm_table <- do.call(rbind, lsm_list)


class_area <- lsm_table %>%
  filter(class == 1, metric == "ca") %>%
  select(row, value)
names(class_area) <- c("id", "class_area")

npatch <- lsm_table %>%
  filter(class == 1, metric == "np") %>%
  select(row, value)
names(npatch) <- c("id", "npatch")

area_mn <- lsm_table %>%
  filter(class == 1, metric == "area_mn") %>%
  select(row, value)
names(area_mn) <- c("id", "area_mn")

area_sd <- lsm_table %>%
  filter(class == 1, metric == "area_sd") %>%
  select(row, value)
names(area_sd) <- c("id", "area_sd")

contag <- lsm_table %>%
  filter(metric == "contag") %>%
  select(row, value)
names(contag) <- c("id", "contag")

ed <- lsm_table %>%
  filter(metric == "ed") %>%
  select(row, value)
names(ed) <- c("id", "ed")

enn_mn <- lsm_table %>%
  filter(metric == "enn_mn") %>%
  select(row, value)
names(enn_mn) <- c("id", "enn_mn")

lpi <- lsm_table %>%
  filter(metric == "lpi") %>%
  select(row, value)
names(lpi) <- c("id", "lpi")


new_metrics <- cbind(class_area, npatch[,2], area_mn[,2], area_sd[,2], contag[,2], ed[,2], enn_mn[,2], lpi[,2])


write.csv(new_metrics, "Data/buffer100_lsm.csv", row.names = FALSE)
