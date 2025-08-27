library("terra")
library("sf")
library("tidyverse")



# Data setup --------------------------------------------------------------

rast2007 <- rast("Outputs/predicted2007.grd")
rast2016 <- rast("Outputs/predicted2016.grd")


transitions <- c(rast2007, rast2016)
transitions <- as.data.frame(transitions, xy = TRUE)
transitions$transition <- paste0(transitions$str2007, " to ", transitions$str2016)

tab <- table(transitions$transition)
tab/nrow(transitions)




# Check non fire zeroes ---------------------------------------------------

#Create a blank template raster
blank_rast <- rast(ext = ext(rast2016), res = res(rast2016), crs = crs(rast2016))


#Load relevant shapefiles
fire_hist <- vect("Spatial/FIRE_HISTORY.shp")
fire_hist <- project(fire_hist, crs(blank_rast))
fire_hist <- crop(fire_hist, blank_rast)

fire_between <- st_as_sf(fire_hist) %>%
  select(SEASON, geometry) %>%
  filter(SEASON >= 2007 & SEASON <= 2016)

fire_between <- rasterize(fire_between, blank_rast, field = "SEASON")


type3s <- rast2016
type3s[type3s < 3] <- NA

type2s <- rast2007
type2s[type2s != 2] <- NA


plot(type3s)
not_burned <- mask(type3s, fire_between, inverse = TRUE)
plot(not_burned)


not_burned_was_2 <- mask(not_burned, type2s)
plot(not_burned_was_2)

not_burned <- as.data.frame(not_burned, xy = TRUE)
not_burned <- na.omit(not_burned)


not_burned_was_2 <- c(not_burned_was_2, rast2007)
not_burned_was_2 <- as.data.frame(not_burned_was_2, xy = TRUE)
not_burned_was_2 <- na.omit(not_burned_was_2)


tab2 <- table(transitions$str2016)
nrow(not_burned)/tab2[3]

nrow(not_burned_was_2)/nrow(not_burned)



# Plot a few --------------------------------------------------------------
set.seed(406)

not_burned_sample <- not_burned %>%
  slice_sample(n=30) %>%
  mutate(ID = seq(1:nrow(not_burned_sample)))

new_rast <- rast(not_burned_sample[,c(1,2,4)], type = "xyz")
crs(new_rast) <- crs(rast2007)


poly <- as.polygons(new_rast, values = TRUE)
poly <- st_as_sf(poly)

write_sf(poly, "Outputs/unburned3s.shp", overwrite = TRUE)

