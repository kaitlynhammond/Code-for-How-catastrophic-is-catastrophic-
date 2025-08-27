library(terra)
library(lidR)
library(tidyverse)
library(sf)
library(ggspatial)
library(patchwork)

zones <- st_read("canopy_zones3.shp")
rast2016 <- rast("outputs/predicted2016.grd")


samples <- c(1860, 1968, 3930)

zones <- zones %>%
  filter(ID %in% samples)

zones$area <- st_area(zones)

#st_write(zones, "transects4patrick.shp")
  
local_1 <- zones[1,]
landscape_1 <- crop(rast2016, local_1)
landscape_1[is.na(landscape_1)]<- 0
landscape_1 <- as.data.frame(landscape_1, xy = TRUE)

local_2 <- zones[2,]
landscape_2 <- crop(rast2016, local_2)
landscape_2[is.na(landscape_2)]<- 0
landscape_2 <- as.data.frame(landscape_2, xy = TRUE)

local_3 <- zones[3,]
landscape_3 <- crop(rast2016, local_3)
landscape_3[is.na(landscape_3)]<- 4
landscape_3 <- as.data.frame(landscape_3, xy = TRUE)




colors <- c("lightgray","#BAE4B3" , "#74C476" , "#238B45" )

a <- ggplot() +
  geom_raster(data = landscape_1, aes(x = x, y = y, fill = factor(str2016))) +
  scale_fill_manual(values = c("lightgray", "#74C476", "#BAE4B3"), guide="none")+
  annotation_scale(location = 'bl', style = 'bar', text_col = "black")+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 22, vjust=-1, hjust =.1),
        text=element_text(size=22),
        plot.title.position = "plot")+ 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  ggtitle("a) High severity") +
  coord_fixed()


b <- ggplot() +
  geom_raster(data = landscape_2, aes(x = x, y = y, fill = factor(str2016))) +
  scale_fill_manual(values = c("lightgray" ,  "#238B45","#74C476" ,"#BAE4B3"), guide="none")+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 22, vjust=-1, hjust =.1),
        text=element_text(size=22),
        plot.title.position = "plot")+ 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  ggtitle("b) Moderate severity")+ 
  coord_fixed()


c <- ggplot() +
  geom_raster(data = landscape_3, aes(x = x, y = y, fill = factor(str2016))) +
  scale_fill_manual(values = c("4" = "lightgray" ,"1" = "#238B45","2" = "#74C476" , "3" = "#BAE4B3"),labels = c("Full canopy", "Partial canopy", "No canopy", "Other forest"))+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 22, vjust=-1, hjust =0.001),
        text=element_text(size=22))+ 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0))+
  labs(fill = "") +
  ggtitle("c) Low severity")+ 
  coord_fixed()



fig <- a + b + c + free(d) + plot_layout(design = layout)
fig




las_659 <- readLAS("Data/plots4ha_transects/659_transect.las")
high <- readLAS("Data/transects4p/1860_transect.las")
mod <- readLAS("Data/transects4p/1968_transect.las")
low <- readLAS("Data/transects4p/3930_transect.las")


df_659 <- as.data.frame(cbind(las_659$X, las_659$Y, las_659$Z))
high <- as.data.frame(cbind(high$X, high$Y, high$Z))
mod <- as.data.frame(cbind(mod$X, mod$Y, mod$Z))
low <- as.data.frame(cbind(low$X, low$Y, low$Z))


names(df_659) <- c("x", "y", "z")
names(high) <- c("x", "y", "z")
names(mod) <- c("x", "y", "z")
names(low) <- c("x", "y", "z")



# random one --------------------------------------------------------------


df_659$d <- df_659$x/df_659$y
max(df_659$d)
min(df_659$d)

282.84271/(0.06895172- 0.06898481)
df_659$test <- df_659$d*8547679
max(df_659$test)
df_659$test <- df_659$test - 589660
df_659$test <- -(df_659$test)
min(df_659$z)
df_659$new_z <- df_659$z - 810.13


d <- ggplot(df_659) +
  geom_point(aes(x=test, y=new_z),  size = .1, stroke= 0) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        text=element_text(size=20),
        axis.title.y = element_text(vjust = 3)) +
  xlab("Distance(m)") +
  ylab("Height (m)") 



layout <- c("
AABBCC
AABBCC
DDDDDD
DDDDDD")

fig <- a + b + c + patchwork::free(d) + plot_layout(design = layout)
fig



 # plot 2 ------------------------------------------------------------------


high$d <- high$x/high$y
max(high$d)
min(high$d)

565.685/(0.06765721- 0.06759222)
high$test <- high$d*8704185
max(high$test)
high$test <- high$test - 588900.8
high$test <- -(high$test)
min(high$z)
high$new_z <- high$z - 585.04



ggplot(high) +
  geom_point(aes(x=test, y=new_z), size = 0.1) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line()) +
  xlab("Distance(m)") +
  ylab("Height (m)")+ 
  coord_fixed()


# Plot3 -------------------------------------------------------------------



mod$d <- mod$x/mod$y
max(mod$d)
min(mod$d)

565.685/( 0.06780323- 0.06773824)
mod$test <- mod$d*8704185
max(mod$test)
mod$test <- mod$test - 590171.9
mod$test <- -(mod$test)
min(mod$z)
mod$new_z <- mod$z - 693.81



ggplot(mod) +
  geom_point(aes(x=test, y=new_z), size = 0.1) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line()) +
  xlab("Distance(m)") +
  ylab("Height (m)")+ 
  coord_fixed()



# Plot 4 ------------------------------------------------------------------

low$d <- low$x/low$y
max(low$d)
min(low$d)

565.685/(0.06735353- 0.06732034)
low$test <- low$d*17043839
max(low$test)
low$test <- low$test -  1147726
low$test <- -(low$test)
min(low$z)
low$new_z <- low$z - 803.52



ggplot(low) +
  geom_point(aes(x=test, y=new_z), size = 0.1) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line()) +
  xlab("Distance(m)") +
  ylab("Height (m)")+ 
  coord_fixed()


