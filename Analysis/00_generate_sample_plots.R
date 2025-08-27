#Filenames can be pure filenames
#Prefix is all the filepath before the las name
#Output is output pathway, no "/"
library(lidR)
library(plotly)
library(rgl)
library(tidyverse)
library(viridis)
library(processx)
library(sf)

plot3D <- function(filenames, prefix, output){
  
  for (i in 1:length(filenames)){
    file <- filenames[i]
    name <- gsub(paste0(prefix), "", (tools::file_path_sans_ext(file)))
    
    #Read the las file
    las <- readLAS(file)
    las <- filter_poi(las, Z>0, Z<90)
    
    #Convert to a data frame
    df <- as.spatial(las)
    df <- as.data.frame(df, xy = TRUE)
    
    
    #Add a column that gives it an assigned color
    df <- df %>% mutate(
      color = case_when ( 0 < Z & Z <= 5 ~ 1,
                          5 < Z & Z <= 10 ~ 2,
                          10 < Z & Z <= 15 ~ 3,
                          15 < Z & Z <= 20 ~ 4,
                          20 < Z & Z <= 25 ~ 5,
                          25 < Z & Z <= 30 ~ 6,
                          30 < Z & Z <= 35 ~ 7,
                          35 < Z & Z <= 40 ~ 8,
                          40 < Z & Z <= 45 ~ 9,
                          45 < Z & Z <= 50 ~ 10,
                          50 < Z & Z <= 55 ~ 11,
                          55 < Z & Z <= 60 ~ 12,
                          60 < Z & Z <= 65 ~ 13,
                          65 < Z & Z <= 70 ~ 14,
                          70 < Z & Z <= 75 ~ 15,
                          75 < Z & Z <= 80 ~ 16))
    
    df$color <- as.factor(df$color)
    
    #List the colors
    colors <- c(viridis(16))
    
    
    #Plot features
    fig1 <- plot_ly(data = df, 
                    x=~X, y=~Y, z = ~Z,
                    color = ~color,
                    colors = colors[1:length(unique(df$color))],
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
                     title = "Height (m)"),
                   camera = list(eye = list(x = 2, y = 2, z = 1.2))
      ))
    
    save_image(fig1, file = paste0(output, "/" ,name, ".png"))
    
  }
  
}



# Generate random plots ---------------------------------------------------

allrasts07 <- read.csv("Data/filtered_2007metrics.csv")
allrasts16 <- read.csv("Data/filtered_2016metrics.csv")

#Group by fire severity to get training data

set.seed(406)

sampleplots <- allrasts07 %>%
  group_by(firesev) %>%
  sample_n(175) %>%
  mutate(plotID = paste0("sev", firesev, "_", 1:175))


#Convert it to a shapefile
samplerast <- rasterFromXYZ(sampleplots, res = 20, crs = crs(allrasts16))
names(samplerast) <- names(sampleplots[3:18])
sampleshape <- rasterToPolygons(samplerast[[1]], n=4, na.rm = TRUE)

sampleshape <- st_as_sf(sampleshape)
sampleshape$plotID <- sampleplots$plotID

st_write(sampleshape, dsn = "Spatial", layer = "sampleplots_0905.shp", driver = "ESRI Shapefile")



# Repeat to create test set -----------------------------------------------

set.seed(808)

#Group by fire severity to get training data
sampleplots <- allrasts07 %>%
  group_by(firesev) %>%
  sample_n(100) %>%
  mutate(plotID = paste0("sev", firesev, "_", 1:100))


#Convert it to a shapefile
samplerast <- rasterFromXYZ(sampleplots, res = 20, crs = crs(allrasts16))
names(samplerast) <- names(sampleplots[3:17])
sampleshape <- rasterToPolygons(samplerast[[1]], n=4, na.rm = TRUE)

sampleshape <- st_as_sf(sampleshape)
sampleshape$plotID <- sampleplots$plotID

st_write(sampleshape, dsn = "Spatial", layer = "sampleplots_0901.shp", driver = "ESRI Shapefile")



# Plot training plots for classification ----------------------------------


files <- paste0("Data/LASfiles2007/", train_plots$plotID, ".las")
prefix <- "Data/LASfiles2007/"
output <- "Data/FilteredPlots/2007/"


plot3D(files, prefix, output)

files <- paste0("Data/LASfiles2016/", train_plots$plotID, ".las")
prefix <- "Data/LASfiles2016/"
output <- "Data/FilteredPlots/2016/"


plot3D(files, prefix, output)
