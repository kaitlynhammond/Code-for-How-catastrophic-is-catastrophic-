library("terra")
library("exactextractr")
library("sf")
library("fasterize")
library("tidyverse")


# Data setup --------------------------------------------------------------


rast2007 <- rast("Outputs/predicted2007.grd")
rast2016 <- rast("Outputs/predicted2016.grd")

transition <- rast("Outputs/transition_rast.grd")

rast2007 <- resample(rast2007, rast2016, method = "near")
transition <- resample(transition, rast2016, method = "near")

fire_boundary <- rast("Spatial/FIRE_SEV09.grd")
fire_boundary <- crop(fire_boundary, rast2007)


rast2016_grid <- mask(rast2016, fire_boundary)

grid <- aggregate(rast2016_grid, fact = 50, fun = min, na.rm = TRUE)
grid[grid > 1] <- 1
plot(grid)

gridpoly <- as.polygons(grid, aggregate = FALSE, values = TRUE, na.rm = TRUE)
centerpoints <- centroids(gridpoly)
centerpoints <- st_as_sf(centerpoints)


plots4ha <- st_buffer(centerpoints, dist = 100, endCapStyle = "SQUARE")
plots4ha$area <- st_area(plots4ha)

buffer400 <- st_buffer(centerpoints, dist = 200, endCapStyle = "SQUARE")
buffer400$area <- st_area(buffer400)

buffer600 <- st_buffer(centerpoints, dist = 300, endCapStyle = "SQUARE")
buffer600$area <- st_area(buffer600)

buffer800 <- st_buffer(centerpoints, dist = 400, endCapStyle = "SQUARE")
buffer800$area <- st_area(buffer800)

buffer1000 <- st_buffer(centerpoints, dist = 500, endCapStyle = "SQUARE")
buffer1000$area <- st_area(buffer1000)





# Get main plot info ------------------------------------------------------
mtnash <- rast("Spatial/mtnash_extent.grd")
mtnash <- crop(mtnash, ext(rast2016))

highsev <- rast2016
highsev[highsev != 3] <- NA
highsev[!is.na(highsev)] <- 1

plot3s <- exact_extract(highsev, plots4ha, force_df= TRUE, fun = "sum")
names(plot3s) <- "threes"
plotmtnash <- exact_extract(mtnash, plots4ha, force_df= TRUE, fun = "sum")
names(plotmtnash) <- "mtnash_plots"

# Extract pre-fire structure within buffers -------------------------------
prefire_fun <-  function(df){
  coverage <- df %>%
    filter(!is.na(value)) %>%
    summarise(coverage = sum(coverage_fraction))
  fullcanopy <- df %>%
    filter(value ==1) %>%
    summarise(fullcanopy = sum(coverage_fraction))
  partialcanopy <- df %>%
    filter(value ==2) %>%
    summarise(partialcanopy = sum(coverage_fraction))
  newdf <- cbind(coverage, fullcanopy, partialcanopy)
  names(newdf) <- c("mtnash", "full2007", "partial2007")
  return(newdf)
}
  
  
plotprefire <- exact_extract(rast2007, plots4ha, summarize_df= TRUE, fun =prefire_fun)


postfire_fun <-  function(df){
  fullcanopy <- df %>%
    filter(value ==1) %>%
    summarise(fullcanopy = sum(coverage_fraction))
  partialcanopy <- df %>%
    filter(value ==2) %>%
    summarise(partialcanopy = sum(coverage_fraction))
  nocanopy <- df %>%
    filter(value ==3) %>%
    summarise(nocanopy = sum(coverage_fraction))
  newdf <- cbind(fullcanopy, partialcanopy, nocanopy)
  names(newdf) <- c("full2016", "partial2016", "none2016")
  return(newdf)
}


plotpostfire <- exact_extract(rast2016, plots4ha, summarize_df= TRUE, fun =postfire_fun)


# Extract number of cells that transitioned -------------------------------

binary_fun <-  function(df){
  transitioned <- df %>%
    filter(value ==12 | value == 13 | value == 23) %>%
    summarise(transitioned = sum(coverage_fraction))
  severe <- df %>%
    filter(value == 13 | value == 23) %>%
    summarise(transitioned = sum(coverage_fraction))
  newdf <- cbind(transitioned, severe)
  names(newdf) <- c("transitioned", "severe")
  return(newdf)
}

plottransition <- exact_extract(transition, plots4ha, summarize_df= TRUE, fun =binary_fun)

# Extract topography based on buffers -------------------------------------


topo <- rast("50cmDTM/Terrain/combined_resample.grd")
topo <- resample(topo, rast2016, method = "near")
northness <- cos(topo[["aspect"]] * pi / 180)
names(northness) <- "north"
eastness <- sin(topo[["aspect"]] * pi / 180)
names(eastness) <- "east"
topo <- c(topo, eastness, northness)

plottopo <- exact_extract(topo, plots4ha, fun = "mean")
plotcovtopo <- exact_extract(topo, plots4ha, fun = "coefficient_of_variation")


# Extract land tenure -----------------------------------------------------

tenures <- rast("Spatial/tenures.grd")
tenures <- resample(tenures, rast2016, method = "near")


ten_fun <- function(df){
  parks <- df %>%
    filter(value ==1) %>%
    summarise(parks = sum(coverage_fraction))
  state <- df %>%
    filter(value ==2) %>%
    summarise(state = sum(coverage_fraction))
  dominant_tenure <- as.data.frame(names(which.max(c(parks, state))))
  newdf <- cbind(parks, state, dominant_tenure)
  names(newdf) <- c("parks", "state", "dom_ten")
  return(newdf)
}

plotten <- exact_extract(tenures, plots4ha, summarize_df = TRUE, fun = ten_fun)

# Extract time since last logging and silv history ------------------------

tsll_rast <-  rast("Spatial/tsll_rast.grd")
tsll_rast <- resample(tsll_rast, rast2016, method = "near")

silv_rast <- rast("Spatial/silv_rast.grd")
silv_rast[silv_rast == 300] <- 0
silv_rast <- resample(silv_rast, rast2016, method = "near")

salvage <- silv_rast
salvage[salvage != 3] <- NA

not_salvage <- mask(tsll_rast, salvage, inverse = TRUE)
not_salvage[is.na(not_salvage)]<- 300


##Time since last logging
tsll_fun <- function(df){
  area_logged <- df %>%
    filter(value < 300) %>%
    summarise(area_logged = sum(coverage_fraction))
  mean <- df %>%
    summarise(mean = mean(value))
  min <- df %>%
    summarise(mean = min(value))
  sum_10 <- df %>%
    filter(value < 10) %>%
    summarise(sum_10 = sum(coverage_fraction))
  sum_20 <- df %>%
    filter(value < 20) %>%
    summarise(sum_10 = sum(coverage_fraction))
  sum_30 <- df %>%
    filter(value < 30) %>%
    summarise(sum_30 = sum(coverage_fraction))
  sum_40 <- df %>%
    filter(value < 40) %>%
    summarise(sum_10 = sum(coverage_fraction))
  sum_50 <- df %>%
    filter(value < 50) %>%
    summarise(sum_10 = sum(coverage_fraction))
  sum_60 <- df %>%
    filter(value < 60) %>%
    summarise(sum_50 = sum(coverage_fraction))
  sum_70 <- df %>%
    filter(value < 70) %>%
    summarise(sum_70 = sum(coverage_fraction))
  newdf <- cbind(area_logged,
                 mean,
                 min,
                 sum_10,
                 sum_20,
                 sum_30,
                 sum_40,
                 sum_50,
                 sum_60,
                 sum_70)
  names(newdf) <- c("area_logged", "mean_tsll", "min_tsll", "log_10", "log_20", "log_30", "log_40", "log_50", "log_60", "log_70")
  return(newdf)
} 

silv_fun <- function(df){
  prop_clearfell <- df %>%
    filter(value == 1) %>%
    summarise(prop_clearfell = sum(coverage_fraction))
  prop_thinning <- df %>%
    filter(value == 2) %>%
    summarise(prop_thinning = sum(coverage_fraction))
  prop_salvage <- df %>%
    filter(value == 3) %>%
    summarise(prop_salvage = sum(coverage_fraction))
  prop_STS <- df %>%
    filter(value == 4) %>%
    summarise(prop_STS = sum(coverage_fraction))
    newdf <- cbind(prop_clearfell,
                 prop_thinning,
                 prop_salvage,
                 prop_STS)
  names(newdf) <- c("clearfell", "salvage", "thinning", "STS")
  return(newdf)
}



plotharv <- exact_extract(not_salvage, plots4ha, summarize_df = TRUE, fun = tsll_fun)
plotsilv <- exact_extract(silv_rast, plots4ha, summarize_df = TRUE, fun = silv_fun)



# Extract fire history ----------------------------------------------------
fire_rast <- rast("Spatial/fire_rast.grd")
fire_rast <- resample(fire_rast, rast2016, method = "near")

  
  
tslf_fun <- function(df){
    mean <- df %>%
      summarise(mean = mean(value))
    min <- df %>%
      summarise(mean = min(value))
    newdf <- cbind(mean,
                   min)
    names(newdf) <- c("mean_tslf", "min_tslf")
    return(newdf)
}

plottslf <- exact_extract(fire_rast, plots4ha, summarize_df = TRUE, fun = tslf_fun)



# EVCs --------------------------------------------------------------------


evc_rast <- rast("Spatial/evc_layer.grd")
evc_rast <- resample(evc_rast, rast2016, method = "near")


evc_fun <- function(df){
  prop_wet <-  df %>%
    filter(value == 7) %>%
    summarise(prop_wet = sum(coverage_fraction))
  prop_dry <- df %>%
    filter(value == 6) %>%
    summarise(prop_dry = sum(coverage_fraction))
  prop_rain <-  df %>%
    filter(value == 8) %>%
    summarise(prop_rain = sum(coverage_fraction))
  prop_rip <-  df %>%
    filter(value == 9) %>%
    summarise(prop_rip = sum(coverage_fraction))
  other_forest <- df %>%
    filter(value == 3| value == 13 |value == 14) %>%
    summarise(other_forest = sum(coverage_fraction))
  woodheath <- df %>%
    filter(value == 16| value == 10 |value == 11 | value == 15 | value == 2 | value == 5 | value == 20) %>%
    summarise(woodheath = sum(coverage_fraction))
  nonveg <- df %>%
    filter(value == 99)%>%
    summarise(nonveg = sum(coverage_fraction))
  
  df <- as.data.frame(cbind(prop_wet,
                              prop_dry,
                              prop_rain,
                              prop_rip,
                              other_forest,
                            woodheath,
                            nonveg))
  names(df) <- c("wet", "dry", "rainfores", "riparian", "otherforest", "woodheath", "nonveg")
  return(df)
}

plotevc <- exact_extract(evc_rast, plots4ha, summarize_df = TRUE, fun = evc_fun)


# TWI ---------------------------------------------------------------------


twi <- rast("Spatial/twi.grd")
twi <- project(twi, rast2007)
twi_crop <- crop(twi, rast2007)
twi_crop <- resample(twi_crop, rast2016, method = "near")

plottwi <- exact_extract(twi_crop, plots4ha, force_df = TRUE, fun = "mean")



# Putting it all together -------------------------------------------------

plots4ha <- cbind(plottransition,
                  plots4ha,
                  plot3s,
                  plotmtnash,
                  plotprefire,
                  plotpostfire,
                  plottopo,
                  plotten,
                  plottwi)

plots4ha <- plots4ha %>%
  select(-str2016)


#Within buffers
#400
pre400 <-  exact_extract(rast2007, buffer400, summarize_df= TRUE, fun =prefire_fun)
post400 <- exact_extract(rast2016, buffer400, summarize_df= TRUE, fun =postfire_fun)
topo400 <- exact_extract(topo, buffer400, fun = "mean")
cov400 <- exact_extract(topo[[1:2]], buffer400, fun = "coefficient_of_variation")
names(cov400) <- c("cov_elev", "cov_slope")
ten400 <- exact_extract(tenures, buffer400, summarize_df= TRUE, fun = ten_fun)
harv400 <- exact_extract(not_salvage, buffer400, summarize_df= TRUE, fun = tsll_fun)
silv400 <- exact_extract(silv_rast, buffer400, summarize_df= TRUE, fun = silv_fun)
tslf400 <- exact_extract(fire_rast, buffer400, summarize_df=TRUE, fun = tslf_fun)
evc400 <- exact_extract(evc_rast, buffer400, summarize_df = TRUE, fun = evc_fun)
twi400 <- exact_extract(twi_crop, buffer400, force_df = TRUE, fun = "mean")

buffer400 <- cbind(buffer400,
                   pre400,
                   post400,
                   topo400,
                   cov400,
                   ten400,
                   harv400,
                   silv400,
                   tslf400,
                   evc400,
                   twi400)


#600
pre600 <-  exact_extract(rast2007, buffer600, summarize_df= TRUE, fun =prefire_fun)
post600 <- exact_extract(rast2016, buffer600, summarize_df= TRUE, fun =postfire_fun)
topo600 <- exact_extract(topo, buffer600, fun = "mean")
cov600 <- exact_extract(topo[[1:2]], buffer600, fun = "coefficient_of_variation")
names(cov600) <- c("cov_elev", "cov_slope")
ten600 <- exact_extract(tenures, buffer600, summarize_df= TRUE, fun = ten_fun)
harv600 <- exact_extract(not_salvage, buffer600, summarize_df= TRUE, fun = tsll_fun)
silv600 <- exact_extract(silv_rast, buffer600, summarize_df= TRUE, fun = silv_fun)
tslf600 <- exact_extract(fire_rast, buffer600, summarize_df=TRUE, fun = tslf_fun)
evc600 <- exact_extract(evc_rast, buffer600, summarize_df = TRUE, fun = evc_fun)
twi600 <- exact_extract(twi_crop, buffer600, force_df = TRUE, fun = "mean")

buffer600 <- cbind(buffer600,
                   pre600,
                   post600,
                   topo600,
                   cov600,
                   ten600,
                   harv600,
                   silv600,
                   tslf600,
                   evc600,
                   twi600)

#800
pre800 <-  exact_extract(rast2007, buffer800, summarize_df= TRUE, fun =prefire_fun)
post800 <- exact_extract(rast2016, buffer800, summarize_df= TRUE, fun =postfire_fun)
topo800 <- exact_extract(topo, buffer800, fun = "mean")
cov800 <- exact_extract(topo[[1:2]], buffer800, fun = "coefficient_of_variation")
names(cov800) <- c("cov_elev", "cov_slope")
ten800 <- exact_extract(tenures, buffer800, summarize_df= TRUE, fun = ten_fun)
harv800 <- exact_extract(not_salvage, buffer800, summarize_df= TRUE, fun = tsll_fun)
silv800 <- exact_extract(silv_rast, buffer800, summarize_df= TRUE, fun = silv_fun)
tslf800 <- exact_extract(fire_rast, buffer800, summarize_df=TRUE, fun = tslf_fun)
evc800 <- exact_extract(evc_rast, buffer800, summarize_df = TRUE, fun = evc_fun)
twi800 <- exact_extract(twi_crop, buffer800, force_df = TRUE, fun = "mean")

buffer800 <- cbind(buffer800,
                   pre800,
                   post800,
                   topo800,
                   cov800,
                   ten800,
                   harv800,
                   silv800,
                   tslf800,
                   evc800,
                   twi800)



#1000
pre1000 <-  exact_extract(rast2007, buffer1000, summarize_df= TRUE, fun =prefire_fun)
post1000 <- exact_extract(rast2016, buffer1000, summarize_df= TRUE, fun =postfire_fun)
topo1000 <- exact_extract(topo, buffer1000, fun = "mean")
cov1000 <- exact_extract(topo[[1:2]], buffer1000, fun = "coefficient_of_variation")
names(cov1000) <- c("cov_elev", "cov_slope")
ten1000 <- exact_extract(tenures, buffer1000, summarize_df= TRUE, fun = ten_fun)
harv1000 <- exact_extract(not_salvage, buffer1000, summarize_df= TRUE, fun = tsll_fun)
silv1000 <- exact_extract(silv_rast, buffer1000, summarize_df= TRUE, fun = silv_fun)
tslf1000 <- exact_extract(fire_rast, buffer1000, summarize_df=TRUE, fun = tslf_fun)
evc1000 <- exact_extract(evc_rast, buffer1000, summarize_df = TRUE, fun = evc_fun)
twi1000 <- exact_extract(twi_crop, buffer1000, force_df = TRUE, fun = "mean")

buffer1000 <- cbind(buffer1000,
                   pre1000,
                   post1000,
                   topo1000,
                   cov1000,
                   ten1000,
                   harv1000,
                   silv1000,
                   tslf1000,
                   evc1000,
                   twi1000)



# Add ID number for plotting ----------------------------------------------

plots4ha$ID <- 1:nrow(plots4ha)
buffer400$ID <- 1:nrow(plots4ha)
buffer600$ID <- 1:nrow(plots4ha)
buffer800$ID <- 1:nrow(plots4ha)
buffer1000$ID <- 1:nrow(plots4ha)





# Write the files ---------------------------------------------------------

st_write(plots4ha, "Data/plot_level.shp")
st_write(buffer400, "Data/buffer400.shp")
st_write(buffer600, "Data/buffer600.shp")
st_write(buffer800, "Data/buffer800.shp")
st_write(buffer1000, "Data/buffer1000.shp")





# Save as csvs ------------------------------------------------------------

plots4ha <- st_drop_geometry(plots4ha)
buffer400 <- st_drop_geometry(buffer400)
buffer600 <- st_drop_geometry(buffer600)
buffer800 <- st_drop_geometry(buffer800)
buffer1000 <- st_drop_geometry(buffer1000)

plots4ha_test <-plots4ha %>%
  select(!geometry)
names(plots4ha_test) <- c(names(plots4ha_test[,1:21]), "TWI")


write.csv(plots4ha_test, "Data/plot_level.csv", row.names = FALSE)
write.csv(buffer400, "Data/buffer400.csv", row.names = FALSE)
write.csv(buffer600, "Data/buffer600.csv", row.names = FALSE)
write.csv(buffer800, "Data/buffer800.csv", row.names = FALSE)
write.csv(buffer1000, "Data/buffer1000.csv", row.names = FALSE)