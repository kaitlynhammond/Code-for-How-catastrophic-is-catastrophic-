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




filenames <- list.files("Outputs/canopy_change_LAS/2007", full.names = TRUE)
prefix <- "Outputs/canopy_change_LAS/2007/"
output <- "Outputs/canopy_change_LAS/plotly2007"

plot3D(filenames, prefix, output)


filenames <- list.files("Outputs/canopy_change_LAS/2016", full.names = TRUE)
prefix <- "Outputs/canopy_change_LAS/2016/"
output <- "Outputs/canopy_change_LAS/plotly2016"


plot3D(filenames, prefix, output)






# Combine PNGs ------------------------------------------------------------


library(magick)
library(tidyverse)


plots2016 <- list.files("outputs/canopy_change_LAS/plotly2016")
plots2007 <- list.files("outputs/canopy_change_LAS/plotly2007")

names2016 <- tools::file_path_sans_ext(plots2016)
names2016 <- substr(names2016,1,nchar(names2016)-5)



names2007 <- tools::file_path_sans_ext(plots2007)
names2007 <- substr(names2007,1,nchar(names2007)-5)



table2016 <- data.frame(cbind(plots2016, names2016))
names(table2016) <- c("file2016", "name")

table2007 <- data.frame(cbind(plots2007, names2007))
names(table2007) <- c("file2007", "name")


matching <- left_join(table2007, table2016)
matching$name <- substr(matching$name, 2, nchar(matching$name))

for(i in 1:nrow(matching)){
  file2007 <- matching[i,1]
  file2016 <- matching[i,3]
  name <- matching[i,2]

  
  image1 <- image_trim(image_read(paste0("Outputs/canopy_change_LAS/plotly2007/", file2007)))
  image2 <- image_trim(image_read(paste0("Outputs/canopy_change_LAS/plotly2016/", file2016)))
  
  combined <- image_append(c(image1, image2))
  image_write(combined, path = paste0("Outputs/canopy_change_LAS/side_by_side/", name, ".png"), format = "png")
  
}






filenames <- list.files("Data/LASfiles2016", full.names = TRUE)
prefix <- "Data/LASfiles2016/"
output <- "Outputs/training_plots"


plot3D(filenames, prefix, output)
