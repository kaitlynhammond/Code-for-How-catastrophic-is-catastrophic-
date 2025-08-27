library(terra)
library(nnet)
library(tidyverse)




set.seed(406)


# data setup --------------------------------------------------------------


data2007 <- read.csv("Data/scaled_2007mets_22_02_2024.csv")
data2016 <- read.csv("Data/scaled_2016mets_22_02_2024.csv")


mod2007 <- readRDS("Outputs/binomial_2007.rds")
mod2016 <- readRDS("Outputs/multinom_2016.rds")

template <- rast("Spatial/mets2016_21_02_2024.grd")



# predict -----------------------------------------------------------------


predictions2007 <- predict(mod2007, data2007, type = "response")
data2007$str2007 <- predictions2007
data2007 <- data2007 %>%
  mutate(str2007 = case_when(str2007 < 0.5 ~ 1,
                             str2007 >=  0.5 ~ 2))

table(data2007$str2007)

rast2007 <- rast(data2007[, c(1,2,14)], type = "xyz")
crs(rast2007) <- crs(template)

plot(rast2007)





predictions2016 <- predict(mod2016, data2016, type = "class")
data2016$str2016 <- predictions2016

table(data2016$str2016)

rast2016 <- rast(data2016[, c(1,2,14)], type = "xyz")
crs(rast2016) <- crs(template)

plot(rast2016)





# Save --------------------------------------------------------------------

raster::writeRaster(raster::raster(rast2007), "Outputs/predicted2007.grd")
raster::writeRaster(raster::raster(rast2016), "Outputs/predicted2016.grd")
