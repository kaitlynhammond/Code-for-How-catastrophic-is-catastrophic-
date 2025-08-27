library("sf")
library("e1071")
library("lidR")
library("tidyverse")
library("terra")
# Sweep folders for classified plots --------------------------------------


#Read the sample plot shapefile
plot_shape <- vect("Spatial/sampleplots_0905.shp")

#Filter plots
study_area <- rast("Data/filtered_2007mets_22_02_2024.grd", lyrs = 1)

filtered <- terra::extract(study_area, plot_shape, bind = TRUE)
filtered <- st_as_sf(filtered)
filtered <- na.omit(filtered)

plot_shape <- filtered

outside_fun <- function(y){
  folder <- y
  
  #Function that sweeps the data folders to get sample plots in each category
  fun <- function(x){
    
    files <- list.files(path = paste0("Data/Sorted_by_others/",folder, "/", x, "/"), pattern = "*.jpeg",
                        full.names = FALSE,  recursive=FALSE)
    
    
    plotID <- data.frame(matrix(nrow = length(files), ncol = 2))
    
    for (i in 1:length(files)){
      file <- files[i]
      name <- tools::file_path_sans_ext(file)
      plotID[i, 1] <- name
      plotID[i, 2] <- x
    }
    return(plotID)
  }
  
  
  #Get the classifications for both years
  transitions <- c("1to1", "1to2", "1to3", "2to2", "2to3")
  
  #2007
  plot_transitions <- lapply(transitions, fun)
  plot_transitions<- do.call(rbind.data.frame, plot_transitions)
  
  
  names(plot_transitions) <- c("plotID", paste0(folder))
  
  return(plot_transitions)
}


folders <- list.dirs(path = "Data/Sorted_by_others", recursive = FALSE, full.names = FALSE)

list <- lapply(folders, outside_fun)
plot_transitions<- list %>%
  reduce(full_join, by = "plotID")



# Get my data for comparison ----------------------------------------------


fun <- function(x){
  
  files <- list.files(path = paste0("Data/Sorting_Template_KH/", x, "/"), pattern = "*.jpeg",
                      full.names = FALSE,  recursive=FALSE)
  
  
  plotID <- data.frame(matrix(nrow = length(files), ncol = 2))
  
  for (i in 1:length(files)){
    file <- files[i]
    name <- tools::file_path_sans_ext(file)
    plotID[i, 1] <- name
    plotID[i, 2] <- x
  }
  return(plotID)
}


#Get the classifications for both years
transitions <- c("1to1", "1to2", "1to3", "2to2", "2to3")

#2007
plot_transitions_KH <- lapply(transitions, fun)
plot_transitions_KH<- do.call(rbind.data.frame, plot_transitions_KH)


names(plot_transitions_KH) <- c("plotID", "Sorting_Template_KH")
plot_transitions_KH <- plot_transitions_KH %>%
  filter(plotID %in% plot_transitions$plotID)

all_data <- list(plot_transitions,
                 plot_transitions_KH) %>%
  reduce(full_join, by = "plotID")

all_data[is.na(all_data)] <- "unknown"

all_data_07 <- data.frame(sapply(all_data[2:10], FUN =function(x) substr(x, 1, nchar(x)-3)))
all_data_07[all_data_07 == "unkn"] <- NA

all_data_16 <- data.frame(sapply(all_data[2:10], FUN =function(x) substr(x, nchar(x), nchar(x))))
all_data_16[all_data_16 == "n"] <- NA

all_data_07 <- cbind(all_data$plotID, all_data_07)
all_data_16 <- cbind(all_data$plotID, all_data_16)

names(all_data_07) <- c("plotID", "AL_07", "BW_07", "CN_07", "GH_07", "MB_07", "PB_07", "SK_07", "XB_07", "KH_07")
names(all_data_16) <- c("plotID", "AL_16", "BW_16", "CN_16", "GH_16", "MB_16", "PB_16", "SK_16", "XB_16", "KH_16")

# Compare transitions between participants --------------------------------

all_data <- all_data %>%
  filter(plotID %in% plot_shape$plotID)


data_long <- pivot_longer(all_data, 2:10, names_to = "participant")
table(data_long$plotID, data_long$value)


count <- data_long %>%
  group_by(plotID) %>%
  count(value) %>%
  slice(which.max(n))


tab <- table(count$value, count$n)
row_sums<- rowSums(tab)
rel_count <- sweep(tab, 1, row_sums, FUN = '/')
rel_count


# Compare structures between participants ---------------------------------

data_long$str2007 <- substr(data_long$value, 1, nchar(data_long$value)-3)
data_long$str2016 <- substr(data_long$value, nchar(data_long$value), nchar(data_long$value))

count_16 <- data_long %>%
  group_by(plotID) %>%
  count(str2016) %>%
  slice(which.max(n))

count_07 <- data_long %>%
  group_by(plotID) %>%
  count(str2007) %>%
  slice(which.max(n))

tab <- table(count_16$str2016, count$n)
row_sums<- rowSums(tab)
rel_count <- sweep(tab, 1, row_sums, FUN = '/')
rel_count

rel_sums <- rel_count[,3:5]
rowSums(rel_sums)


tab <- table(count_07$str2007, count$n)
row_sums<- rowSums(tab)
rel_count <- sweep(tab, 1, row_sums, FUN = '/')
rel_count

rel_sums <- rel_count[,3:5]
rowSums(rel_sums)



# Compare before/after separately -----------------------------------------

select2007 <- all_data_07 %>%
  filter(plotID %in% plot_shape$plotID) %>%
  mutate_if(is.character, as.factor)
  

select2007$most_common <- apply(select2007[,2:10], 1,function(x) names(which.max(table(x))))
select2007$percent_chosen <- rowSums(select2007[,2:10] == select2007[,11], na.rm = TRUE)/9



select2016 <- all_data_16 %>%
  filter(plotID %in% plot_shape$plotID) %>%
  mutate_if(is.character, as.factor)


select2016$most_common <- apply(select2016[,2:10], 1,function(x) names(which.max(table(x))))
select2016$percent_chosen <- rowSums(select2016[,2:10] == select2016[,11], na.rm = TRUE)/9


tab1 <- table(select2007$percent_chosen)
tab2 <- table(select2016$percent_chosen)

tab1
tab2
#There was at least 88% agreement for:
sum(tab1[c(3,4,5,6)])/166 #68%
sum(tab2[c(3,4,5,6)])/166 #58%


# Calculate number who agreed with me -------------------------------------

agree2007 <- as.data.frame(apply(select2007[,2:9], 2, function(x) ifelse(x == select2007$KH_07, 1, 0)))
agree2007$matches <- rowSums(agree2007, na.rm = TRUE)/8

agree2007 <- agree2007 %>%
  filter(matches >= .6)


agree2016 <- as.data.frame(apply(select2016[,2:9], 2, function(x) ifelse(x == select2016$KH_16, 1, 0)))
agree2016$matches <- rowSums(agree2016, na.rm = TRUE)/8

agree2016 <- agree2016 %>%
  filter(matches >= .6)


nrow(agree2007)/166 #68%
nrow(agree2016)/166 #57%



# Add the classifications to the training data ----------------------------

sorted <- plot_shape %>%
  filter(plotID %in% plot_transitions$plotID) %>%
  select(-canopy_het.1)



# Calculate the metrics 2016 ----------------------------------------------


#Folders and input
LASfiles <- "Data/LASfiles2016"
output <- "Data/Metrics2016"

files <- list.files(path = paste0(LASfiles), pattern = "*.las",
                    full.names = FALSE,  recursive=FALSE)
files <- tools::file_path_sans_ext(files)
#files <- files[files %in% sorted$plotID]

plot_names <- files

filefun <- function(x){
  paste0(LASfiles, "/",  x, ".las")
}


files <- lapply(files, filefun)
files <- unlist(files)



# Generate metrics --------------------------------------------------------


#Get the catalog info to set up the template raster
ctg <- readLAScatalog(files)
ext <- extent(ctg)
rm(ctg)
fullr1 <- raster(ext, res = 20, crs = "+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#Run the metrics calc (sourced aba_metrics)

source("Scripts/aba_metrics.R")
allrasts <- lapply(files, fun)
merged <- lapply(allrasts, as.data.frame, xy = TRUE)
mets_2016 <- do.call(rbind, merged)
mets_2016$plotID <- as.factor(plot_names)




fun <- function(x){
  
  files <- list.files(path = paste0("Data/Sorting_Template_KH/", x, "/"), pattern = "*.jpeg",
                      full.names = FALSE,  recursive=FALSE)
  
  
  plotID <- data.frame(matrix(nrow = length(files), ncol = 2))
  
  for (i in 1:length(files)){
    file <- files[i]
    name <- tools::file_path_sans_ext(file)
    plotID[i, 1] <- name
    plotID[i, 2] <- x
  }
  return(plotID)
}


#Get the classifications for both years
transitions <- c("1to1", "1to2", "1to3", "2to2", "2to3")

plot_transitions_KH <- lapply(transitions, fun)
plot_transitions_KH<- do.call(rbind.data.frame, plot_transitions_KH)


names(plot_transitions_KH) <- c("plotID", "Sorting_Template_KH")


kh_data_07 <- plot_transitions_KH %>%
  mutate(str2007 = substr(Sorting_Template_KH, 1, nchar(Sorting_Template_KH) -3))
kh_data_07[kh_data_07 == "unkn"] <- NA

kh_data_16 <- plot_transitions_KH %>%
  mutate(str2016 = substr(Sorting_Template_KH, nchar(Sorting_Template_KH), nchar(Sorting_Template_KH)))
kh_data_16[kh_data_16 == "n"] <- NA

mets_2016 <- full_join(mets_2016, kh_data_16[, c(1,3)])
mets_2016 <- na.omit(mets_2016)


#Write csv
write.csv(mets_2016, file = "Data/training_data2016_full.csv", row.names = FALSE)

# Calculate the metrics 2007 ----------------------------------------------


#Folders and input
LASfiles <- "Data/LASfiles2007"
output <- "Data/Metrics2007"

files <- list.files(path = paste0(LASfiles), pattern = "*.las",
                    full.names = FALSE,  recursive=FALSE)
files <- tools::file_path_sans_ext(files)


filefun <- function(x){
  paste0(LASfiles, "/",  x, ".las")
}


files <- lapply(files, filefun)
files <- unlist(files)



# Generate metrics --------------------------------------------------------


#Get the catalog info to set up the template raster
ctg <- readLAScatalog(files)
ext <- extent(ctg)
rm(ctg)
fullr1 <- raster(ext, res = 20, crs = "+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#Run the metrics calc (sourced aba_metrics)

source("Scripts/aba_metrics.R")
allrasts <- lapply(files, fun)
merged <- lapply(allrasts, as.data.frame, xy = TRUE)
mets_2007 <- do.call(rbind, merged)
mets_2007$plotID <- as.factor(plot_names)

mets_2007 <- full_join(mets_2007, kh_data_07[, c(1,3)])
mets_2007 <- na.omit(mets_2007)



#Write csv
write.csv(mets_2007, file = "Data/training_data2007_full.csv", row.names = FALSE)




