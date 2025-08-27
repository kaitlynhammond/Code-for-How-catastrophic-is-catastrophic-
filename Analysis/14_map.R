library(terra)
library(sf)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(ggspatial)
library(patchwork)
library(ggnewscale)
library(spatialEco)





# Data --------------------------------------------------------------------

mtnash <- rast("Spatial/mtnash_extent.grd")


firesev <- vect("Spatial/FIRE_SEV09_POLY.shp")
firesev <- project(firesev, crs(mtnash))
firesev <- st_as_sf(firesev) %>%
mutate(fire_class = factor(case_when(CLASS == 1 ~ "Crown Burn",
                                     CLASS == 2 ~ "Crown Scorch",
                                     CLASS == 3 ~ "Moderate Crown Scorch",
                                     CLASS == 4 ~ "Understorey Burnt",
                                     CLASS == 5 ~ "Understorey Burnt"), 
                           levels = c("Crown Burn","Crown Scorch", "Moderate Crown Scorch", "Understorey Burnt"))) %>%
  filter(!is.na(fire_class))


lidar2007 <- vect("Spatial/Lidar2007_boundary_name.shp")
lidar2007 <- project(lidar2007, crs(mtnash))
lidar2007 <- rasterize(lidar2007, mtnash)

lidar2015 <- vect("Spatial/Lidar2015_boundary_name.shp")
lidar2015 <- project(lidar2015, crs(mtnash))
lidar2015 <- rasterize(lidar2015, mtnash)


mtnash <- mask(mtnash, lidar2015)
mtnash <- as.polygons(mtnash, dissolve=TRUE)
mtnash <- st_as_sf(mtnash)
mtnash$ashcover <- "Mountain ash distribution"

lidar2007 <- mask(lidar2007, lidar2015)


lidar2007 <- as.polygons(lidar2007, dissolve=TRUE)
lidar2007 <- st_as_sf(lidar2007)
lidar2007$coverage <- "Study area "

lidar2015 <- as.polygons(lidar2015, dissolve=TRUE)
lidar2015 <- st_as_sf(lidar2015)

firesev <- st_intersection(firesev, lidar2015)

australia <- read_sf("Spatial/STE_2021_AUST_GDA2020.shp")
australia <- australia %>%
  mutate(is_vic = ifelse(STE_NAME21 == "Victoria", 1, 0)) %>%
  filter((STE_NAME21 != "Other Territories" & STE_NAME21 != "Outside Australia"))

victoria <- australia %>% filter(STE_NAME21 == "Victoria")

melbourne <- st_as_sf(data.frame(y = -37.813628, x = 144.963058), coords = c("x", "y"), crs = "WGS84")
melbourne$name = "Melbourne"



# Inset --------------------------------------------------------------------

inset <- ggplot()+
  geom_sf(data = australia, aes(fill = is_vic),lwd = 1)+
  scale_fill_gradient(low = "grey90", high = "grey40")+
  theme_void() +
  theme(legend.position="none")+
  coord_sf(crs = 4326)


vicmap <- ggplot()+
  geom_sf(data = victoria, fill = "grey90", lwd = 1)+
  geom_sf(data = lidar2015, fill = "grey40") +
  geom_sf(data = melbourne, shape = 15)+
  geom_sf_text(data = melbourne, aes(label = name), size = 5, nudge_y = .3, nudge_x = -.6) +
  theme_void() +
  theme(legend.position="none")

inset2 <- inset <- ggplot()+
  geom_sf(data = australia, aes(fill = is_vic),lwd = 1)+
  geom_sf(data =  st_as_sfc(st_bbox(lidar2015)), color = "red", fill = NA, lwd = 1) +
  scale_fill_gradient(low = "grey90", high = "grey40")+
  theme_void() +
  theme(legend.position="none")+
  coord_sf(crs = 4326)


# LidarCoverage -----------------------------------------------------------

lidarmap <- ggplot()+
  geom_sf(data = lidar2015, lwd = 1) +
  geom_sf(data = mtnash, aes(fill = ashcover), color = NA) +
  scale_fill_manual(values = "#35B779FF") +
  new_scale_fill() +
  geom_sf(data = lidar2007, aes(fill = coverage), color = NA, alpha = .3) +
  scale_fill_manual(values = "#39568CFF") +
  theme(legend.position = c(0.8, 0.9),
        legend.title=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        legend.background = element_blank(),
        legend.spacing.y = unit(0, 'cm'),
        legend.margin = margin(-0.05,0,0,0, unit="cm"),
        text=element_text(size=22),
        plot.title = element_text(size=22),
        legend.text = element_text(size = 16)) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_orienteering,
                         pad_x = unit(1,"cm"),
                         pad_y = unit(1.5,"cm"))+
  annotation_scale(location = 'bl', style = 'ticks', text_col = 'black',
                   pad_x = unit(.5,"cm"),
                   pad_y = unit(.5,"cm"),
                   text_cex = 1) +
  labs(title="A) Species distribution and study area")

lidarmap


# Fire severity mapping ---------------------------------------------------


firemap <- ggplot()+
  geom_sf(data = lidar2015) +
  geom_sf(data = firesev, aes(fill = fire_class), color = NA) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1)+
  guides(fill = guide_legend(nrow = 4))+
  new_scale_fill() +
  geom_sf(data = lidar2015, fill = NA, lwd = 1) +
  new_scale_fill() +
  geom_sf(data = lidar2007, aes(fill = coverage), color = NA, alpha = .3) +
  scale_fill_manual(values = "#39568CFF", guide = "none") +
  theme(legend.position = c(0.82, 0.95),
        legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "transparent"),
        text=element_text(size=22),
        legend.margin = margin(1.5,0,0,0, unit="cm"),
        plot.title = element_text(size=22),
        legend.text = element_text(size = 16))+
  labs(title="B) Fire severity")

firemap


# Compose plots -----------------------------------------------------------


layout <- "
ABBBCCC
#BBBCCC
#BBBCCC
"

map1 <- inset / vicmap

map2 <- lidarmap + firemap + plot_layout(design = layout)

map3 <- inset2 + lidarmap + firemap + plot_layout(design = layout)


map3

library(magick)
new <- image_trim(image_read("rplot06.png"))
image_write(new, path = "inset.png", format = "png")



inset2
