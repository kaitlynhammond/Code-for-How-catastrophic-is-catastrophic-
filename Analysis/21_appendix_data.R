library(tidyverse)



plots <- read.csv("Data/plot_level.csv") %>%
  select(transitioned, severe, mtnash, partial2007, mean.elevation, mean.slope, mean.east, mean.north, parks, TWI, NA.)
names(plots) <- c(names(plots)[1:10], "ID")

buffer400 <- read.csv("Data/buffer400.csv") %>% select(!mtnash)
buffer600 <- read.csv("Data/buffer600.csv") %>% select(!mtnash)
buffer800 <- read.csv("Data/buffer800.csv") %>% select(!mtnash)
buffer1000 <- read.csv("Data/buffer1000.csv") %>% select(!mtnash)


plot_summary <- plots %>% 
  summarise(across(everything(), sd))


summary_400 <- buffer400 %>% 
  summarise(across(everything(), sd))


summary_600 <- buffer600 %>% 
  summarise(across(everything(), sd))

summary_800 <- buffer800 %>% 
  summarise(across(everything(), sd))

summary_1000 <- buffer1000 %>% 
  summarise(across(everything(), sd))
