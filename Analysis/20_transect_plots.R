library(lidR)
library(tidyverse)
library(viridis)
library(tools)

files <- list.files("Data/plots4ha_transects/", full.names = TRUE)
files_n <- list.files("Data/plots4ha_transects/", full.names = TRUE, pattern = "*n_transect.las")
files <- as.data.frame(files) %>%
  filter(!files %in% files_n) %>%
  mutate(name = basename(file_path_sans_ext(files)))
files_n <- as.data.frame(files_n) %>%
  mutate(name = basename(file_path_sans_ext(files_n)))


for(i in 1:nrow(files)){
current <- files[i,1]
current_name <- files[i,2]
height <- files_n[i,1]
las <- readLAS(current)
norm <- readLAS(height)
df <- as.data.frame(cbind(las$X, las$Y, las$Z, norm$Z))
names(df) <- c("x", "y", "z", "h")


p <- ggplot(df) +
  geom_point(aes(x=(x/y), y=z), size = 0.1) +
  theme_void()

p2 <- ggplot(df) +
  geom_point(aes(x=(x/y), y=h), size = 0.1) +
  theme_void()

ggsave(plot = p, filename = paste0("Data/plots4ha_transects_viz/", current_name, ".jpeg"))
ggsave(plot = p2, filename = paste0("Data/plots4ha_transects_viz/n", current_name, ".jpeg"))
}