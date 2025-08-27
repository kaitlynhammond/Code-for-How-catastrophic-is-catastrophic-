library("terra")
library("exactextractr")
library("sf")
library("fasterize")
library("tidyverse")

####NOTE THAT I CHANGED THE DATES TO 8-44 YEARS BUT DIDN'T CHANGE THE OBJECTS
# Data setup --------------------------------------------------------------

rast2016 <- rast("Outputs/predicted2016.grd")

buffer400 <- st_read("Data/buffer400.shp")
buffer600 <- st_read("Data/buffer600.shp")
buffer800 <- st_read("Data/buffer800.shp")
buffer1000 <- vect("Data/buffer1000.shp")

log_hist <- vect("Spatial/lastlog25.shp")
log_hist <- project(log_hist, buffer1000)

log_hist <- st_as_sf(log_hist)
buffer1000 <- st_as_sf(buffer1000)

log_hist <- log_hist %>%
  mutate(year = as.numeric(substr(SEASON, 1 , 4))+1) %>%
  filter(year >= 1965 & year <= 2001) %>%
  mutate(newval = 1)

log_hist <- vect(log_hist)

log_rast <- rasterize(log_hist, rast2016, field = "newval")

area400_15_40 <- exact_extract(log_rast, buffer400, "count", append_cols = TRUE)
area600_15_40 <- exact_extract(log_rast, buffer600, "count", append_cols = TRUE)
area800_15_40 <- exact_extract(log_rast, buffer800, "count", append_cols = TRUE)
area1000_15_40 <- exact_extract(log_rast, buffer1000, "count", append_cols = TRUE)


write_csv(area400_15_40, "Data/buffer400_harv15_40.csv")
write_csv(area600_15_40, "Data/buffer600_harv15_40.csv")
write_csv(area800_15_40, "Data/buffer800_harv15_40.csv")
write_csv(area1000_15_40, "Data/buffer1000_harv15_40.csv")