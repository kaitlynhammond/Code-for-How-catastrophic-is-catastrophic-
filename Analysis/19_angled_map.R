library(lidR)
library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)
library(plotly)
library(viridis)
las <- readLAS("Data/n18_2016.las")

las <- filter_poi(las, Z>0, Z<90)

#Convert to a data frame
df <- as.spatial(las)
df <- as.data.frame(df, xy = TRUE)
df$color <- 1


fig1 <- plot_ly(data = df, 
                x=~X, y=~Y, z = ~Z,
                color = ~color,
                colors = "#BAE4B3", 
                showlegend = FALSE,
                type = "scatter3d",
                mode = "markers",
                marker = list(size = 1.2,
                              line = list(
                                width = 0),
                              width = 500,
                              height = 800)
)

fig1 <- fig1 %>% layout(
  autosize = TRUE,
  scene = list(aspectmode = "manual",
               aspectratio = list(
                 x = 1,
                 y = 1,
                 z = 1
               ),
               xaxis = list(
                 nicks = 5,
                 showticklabels = FALSE,
                 title = ""),
               yaxis = list(
                 nicks = 5,
                 showticklabels = FALSE,
                 title = ""),
               zaxis = list(
                 nicks = 7,
                 range = c(0, 70),
                 showticklabels = FALSE,
                 title = ""),
               camera = list(eye = list(x = 2, y = 2, z = 1.2))
  ))

fig1





buffer1000 <- st_read("Data/buffer1000.shp")%>%
  select(ID, geometry)
buffer400 <- st_read("Data/buffer400.shp")%>%
  select(ID, geometry)
buffer600 <- st_read("Data/buffer600.shp")%>%
  select(ID, geometry)
buffer800 <- st_read("Data/buffer800.shp")%>%
  select(ID, geometry)
plots <- st_read("Data/plot_level.shp")%>%
  select(ID, geometry)

rast2016 <- rast("Outputs/predicted2016.grd")

local <- buffer1000[605,]
plot4ha <- plots[605,]

outline_p <- plots[605,]%>%
  select(ID, geometry)
outline_400<- buffer400[605,]%>%
  select(ID, geometry)
outline_600<- buffer600[605,]%>%
  select(ID, geometry)
outline_800<- buffer800[605,]%>%
  select(ID, geometry)
outline_1000<- buffer1000[605,]%>%
  select(ID, geometry)


sm = matrix(c(2,1.2,0,1),2,2)

outline_p$geometry <- outline_p$geometry * sm
outline_400$geometry <- outline_400$geometry * sm
outline_600$geometry <- outline_600$geometry * sm
outline_800$geometry <- outline_800$geometry * sm
outline_1000$geometry <- outline_1000$geometry * sm


landscape <- crop(rast2016, local)
plot <- crop(rast2016, plot4ha)
plot[is.na(plot)] <- 0
plot<- as.polygons(plot, aggregate = FALSE)
plot <- st_as_sf(plot) %>%
  mutate(ID = 1:nrow(plot))

landscape[is.na(landscape)] <- 0
landscape<- as.polygons(landscape, aggregate = FALSE)
landscape <- st_as_sf(landscape) %>%
  mutate(ID = 1:nrow(landscape))







new = plot$geometry * sm
new2 <- landscape$geometry * sm
plot$geometry <- new
landscape$geometry <- new2
ten <- viridis
colors <- c("lightgray","#BAE4B3" ,  "#74C476"  , "#238B45" )
brewer.pal(6, "Blues")
outlines <- c("#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C")

ggplot() +
  geom_sf(data= plot, aes( fill=factor(str2016)), colour = "gray") +
  scale_fill_manual(values = colors ) +
  theme_void() +
  theme(legend.position = "none")+
  geom_sf(data = outline_p, fill =NA, colour = "#08519C", lwd = 2)


ggplot() +
  geom_sf(data= landscape, aes( fill=factor(str2016)), colour = "gray") +
  scale_fill_manual(values = colors ) +
  theme_void() +
  theme(legend.position = "none") +
  geom_sf(data = outline_p, fill =NA, colour = "#08519C", lwd = 2) +
  geom_sf(data = outline_400, fill =NA, colour = "#3182BD", lwd = 2) +
  geom_sf(data = outline_600, fill =NA, colour = "#6BAED6", lwd = 2) +
  geom_sf(data = outline_800, fill =NA, colour = "#9ECAE1", lwd = 2) +
  geom_sf(data = outline_1000, fill =NA, colour = "#C6DBEF", lwd = 2)
  