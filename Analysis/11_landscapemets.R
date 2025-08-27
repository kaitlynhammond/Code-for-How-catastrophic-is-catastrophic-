library("landscapemetrics")
library("landscapetools")
library("terra")
library("sf")
library("viridis")
library("tidyverse")
library("patchwork")
library("scales")


set.seed(406)

# Data setup --------------------------------------------------------------

rast2007 <- rast("Outputs/predicted2007.grd")
rast2016 <- rast("Outputs/predicted2016.grd")


logged <- rast("Spatial/log_year.grd")
logged[logged < 1977] <- NA

rast2007 <- mask(extend(rast2007, logged), logged, inverse = TRUE)
rast2016 <- mask(extend(rast2016, logged), logged, inverse = TRUE)

fire_boundary <- rast("Spatial/FIRE_SEV09.grd")
fire_boundary <- crop(fire_boundary, rast2007)

lidar2015 <- vect("Spatial/Lidar2015_boundary_name.shp")
lidar2015 <- project(lidar2015, crs(rast2007))
lidar2015 <- rasterize(lidar2015, rast2007)

rast2007 <- mask(rast2007, fire_boundary)
rast2016 <- mask(rast2016, fire_boundary)

df <- c(rast2007, rast2016, fire_boundary)
df <- as.data.frame(df, xy = TRUE)
df$transition <- paste0(df$str2007, " to ", df$str2016)

transition_rast <- df %>%
  filter(!is.na(str2007), !is.na(CLASS)) %>%
  mutate(code = case_when(transition == "1 to 1" ~ 11,
                          transition == "1 to 2" ~ 12,
                          transition == "1 to 3" ~ 13,
                          transition == "2 to 1" ~ 21,
                          transition == "2 to 2" ~ 22,
                          transition == "2 to 3" ~ 23))
transition_rast <- rast(transition_rast[c(1,2,7)], type = "xyz")


binary_rast <- df %>%
  filter(!is.na(str2007), !is.na(CLASS)) %>%
  mutate(code = case_when(transition == "1 to 1" ~ 0,
                          transition == "1 to 2" ~ 1,
                          transition == "1 to 3" ~ 1,
                          transition == "2 to 1" ~ 0,
                          transition == "2 to 2" ~ 0,
                          transition == "2 to 3" ~ 1))
binary_rast <- rast(binary_rast[c(1,2,7)], type = "xyz")

# Transition matrices -----------------------------------------------------

#Get a table of transitions for whole landscape
transitions <- df %>%
  filter(!is.na(str2007))

tab <- table(transitions$transition)
tab/nrow(transitions)


#Get table of transitions for the fire footprint
footprint <- df %>%
  filter(!is.na(str2007), !is.na(CLASS))

tab <- table(footprint$transition)
tab/nrow(footprint)

#Table of transitions for type 1
ones <- df %>%
  filter(!is.na(str2007), !is.na(CLASS), transition == "1 to 1" | transition == "1 to 2" | transition == "1 to 3" | transition  == "2 to 1")

tab <- table(ones$transition)
tab/nrow(ones)


#Table of transitions for type 2
twos <- df %>%
  filter(!is.na(str2007), !is.na(CLASS), transition == "2 to 2" | transition == "2 to 3")

tab <- table(twos$transition)
tab/nrow(twos)


#Prefire:
table(footprint$str2007)/nrow(footprint)


#Total area burned
tab <- table(transitions$transition)
sum(tab[c(2,3,6)])/nrow(footprint)

#Total area no canopy left
sum(tab[c(3,6)])/nrow(footprint)

#Total area partially burnt
sum(tab[2])/nrow(footprint)


#Get transitions for each severity level
sev_by_level <- footprint %>%
  filter(CLASS != 6) %>%
  group_by(CLASS) %>%
  summarise(total_burnt = sum(transition == "1 to 2" |
                                  transition == "1 to 3" |
                                  transition == "2 to 3"),
            total = n()) %>%
  mutate(prop_burnt = total_burnt/total)
sev_by_level


hectares_summary <- footprint %>%
  group_by(transition) %>%
  summarise(n = n()) %>%
  mutate(area = n*400/10000)
hectares_summary


hectares_summary <- footprint %>%
  group_by(str2007) %>%
  summarise(n = n()) %>%
  mutate(area = n*400/10000)
hectares_summary

hectares_summary <- footprint %>%
  group_by(str2016) %>%
  summarise(n = n()) %>%
  mutate(area = n*400/10000)
hectares_summary


burnt_by_class <- footprint %>%
  filter(transition == "1 to 2" | transition == "1 to 3" | transition == "2 to 3")
nrow(burnt_by_class)*400/10000

burnt_by_class <- burnt_by_class %>%
  group_by(transition) %>%
  summarise(n = n()) %>%
  mutate(area = n*400/10000)


sev_loss_est <- as.data.frame(mask(fire_boundary, lidar2015)) %>%
  filter(CLASS == 1)%>%
  summarise(n = n()) %>%
  mutate(area = n*400/10000)
sev_loss_est

mod_loss_est <- as.data.frame(mask(fire_boundary, lidar2015)) %>%
  filter(CLASS > 1)%>%
  summarise(n = n()) %>%
  mutate(area = n*400/10000)
mod_loss_est
# Entry size distribution -------------------------------------------------
#Get size distribution for harvested patches in the past 50 years
loghist <- st_read("Spatial/LASTLOG25.shp")
loghist <- loghist %>% 
  mutate(year = as.numeric(substr(SEASON, 1 , 4))+1, 
         area = as.numeric(st_area(loghist)/10000),
         history = "harvest")
logarea <- loghist %>%
  filter(year >= 1970, X_FORETYPE == "Mountain Ash",
         X_SILVSYS == "Clearfelling" |
           X_SILVSYS == "Seed Tree (includes retained overwood)")
logarea <- logarea[,26:27]
logarea <- st_set_geometry(logarea, NULL)



#Calculate 
patches16 <-lsm_p_area(binary_rast, directions = 8)
patches16 <- patches16 %>%
  filter(class == 1)

over50 <- patches16 %>%
  filter(value >50)

names(patches16)[names(patches16) == "value"] <- "area"
patches16$history <- "fire"
patches16 <- patches16[,6:7]


patches <- rbind(patches16, logarea)

colors <- c(viridis(5))


ggplot(patches, aes(x=area, color = history, fill = history)) +
  geom_histogram(alpha = .6)+
  scale_x_log10()+
  scale_fill_manual(values = c("#5DC863FF", "#3B528BFF" )) +
  scale_color_manual(values = c("#5DC863FF", "#3B528BFF")) +
  annotation_logticks(sides = "b",
                      outside = TRUE)
  
max(logarea$area)

# Canopy cover ------------------------------------------------------------

#Create a raster with canopy cover by value
canopy2007 <- mask(rast2007, fire_boundary)
canopy2007[canopy2007 ==2] <- .5
canopy2007[is.na(canopy2007)] <- 0

canopy2016 <- mask(rast2016, fire_boundary)
canopy2016[canopy2016 ==2] <- .5
canopy2016[canopy2016 ==3] <- 0
canopy2016[is.na(canopy2016)] <- 0

#Aggregate to get canopy cover
canopy2007 <- aggregate(canopy2007, fact = 10, fun = sum)
canopy2007[canopy2007 == 0] <- NA

canopy2016 <- aggregate(canopy2016, fact = 10, fun = sum)
canopy2016 <- mask(canopy2016, canopy2007)

#Max canopy cover
coverage <- rast2007
coverage[coverage >0] <- 1
coverage <- aggregate(coverage, fact = 10, fun = sum, na.rm = TRUE)

#Divide by maximum canopy coverage per aggregated pixel
canopy2007 <- canopy2007/coverage
canopy2016 <- canopy2016/coverage

plot(canopy2007)
plot(canopy2016)

canopy07 <- as.data.frame(canopy2007)
canopy16 <- as.data.frame(canopy2016) %>%
  mutate(group = case_when(str2016 < .1  ~ "high",
                           str2016 >= 0.1 & str2016 < .8 ~ "mod",
                           str2016 >= 0.8 ~ "low"))
summary <- canopy16 %>%
  group_by(group) %>%
  summarise(n = n())
summary

canopy_change <- canopy2016-canopy2007
plot(canopy_change)

canopy_change_df <- as.data.frame(canopy_change, xy = TRUE) %>%
  mutate(str2016 = ifelse(str2016 >= 0, 0, -1*str2016))
names(canopy_change_df) <- c("x", "y", "Canopy_loss")



ggplot(canopy_change_df, aes(x = Canopy_loss, y = after_stat(count)/sum(after_stat(count)))) +
  geom_histogram(color = "dodgerblue", fill = "dodgerblue", alpha = .8, expand = c(0,0))+
  labs(y= "Proportion", x = "Proportion of canopy loss per 4-ha block")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
  



canopy_zones <- as.data.frame(canopy2016, xy = TRUE)
canopy_zones <- canopy_zones %>%
  mutate(zone = ifelse(str2016 <= .1, 1, ifelse(str2016 > .1 & str2016 <= .8, 2, 3)))
canopy_zones <- rast(canopy_zones[c(1,2,4)], type = "xyz")
crs(canopy_zones) <- crs(rast2016)


tenures <- rast("Spatial/tenures.grd")
tenures <- resample(tenures, canopy_zones, method = "near")

check <- c(tenures, canopy_zones)
check <- as.data.frame(check, xy = TRUE)
check <- check %>%
  filter(!is.na(zone)) %>%
  group_by(layer, zone) %>%
  summarise(n = n())


polygons <- as.polygons(canopy_zones, aggregate = FALSE)
polygons$ID <- 1:nrow(polygons)
crs(polygons) <- crs(rast2016)
polygons <- st_as_sf(polygons)
polygons$area <- st_area(polygons)
#writeVector(polygons, "canopy_zones3.shp")




canopy_zones <- disagg(canopy_zones, fact = 10)
canopy_zones <- extend(canopy_zones, binary_rast)
binary_rast <- extend(binary_rast, canopy_zones)



lowsev <- canopy_zones
lowsev[lowsev!= 3] <- NA
lowsev <- mask(binary_rast, lowsev)


modsev <- canopy_zones
modsev[modsev!=2] <-NA
modsev <- mask(binary_rast, modsev)


highsev <- canopy_zones
highsev[highsev!=1] <- NA
highsev <- mask(binary_rast, highsev)


patches_high <-lsm_p_area(highsev, directions = 8)
patches_high <- patches_high %>%
  filter(class == 0)

names(patches_high)[names(patches_high) == "value"] <- "area"
patches_high$zone <- "High severity"
patches_high$color <- "a"

patches_mod <-lsm_p_area(modsev, directions = 8)
patches_mod <- patches_mod %>%
  filter(class == 0)

names(patches_mod)[names(patches_mod) == "value"] <- "area"
patches_mod$zone <- "Moderate severity"
patches_mod$color <- "a"


patches_low <-lsm_p_area(lowsev, directions = 8)
patches_low <- patches_low %>%
  filter(class == 1)

names(patches_low)[names(patches_low) == "value"] <- "area"
patches_low$zone <- "Low severity"
patches_low$color <- "b"



all_zones <- rbind(patches_high, patches_mod, patches_low)
all_zones <- all_zones %>%
  mutate(zone = factor(zone, levels = c("High severity", "Moderate severity", "Low severity")),
         class = factor(class))

a <- ggplot(all_zones %>% filter(zone == "High severity"), aes(x = area, y = after_stat(count / sum(count)), fill = "forestgreen")) +
  geom_histogram(bins = 20) +
  scale_x_log10(breaks= c(0.01,.1,1,10,100,1000), limits = c(0.01, 10000), labels = label_number(), expand = c(0,0)) +
  scale_fill_manual(guide = 'none', values = c("forestgreen"))+
  labs(y= "Proportion of patches", x = "") + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(c(0, 0.8))


b <- ggplot(all_zones %>% filter(zone == "Moderate severity"), aes(x = area, y = after_stat(count / sum(count)), fill = "forestgreen")) +
  geom_histogram(bins = 20) +
  scale_x_log10(breaks= c(0.01,.1,1,10,100,1000), limits = c(0.01, 10000), labels = label_number(), expand = c(0,0)) +
  scale_fill_manual(guide = 'none', values = c("forestgreen"))+
  labs(y= "", x = "Area (ha)") + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(c(0, 0.8))


c <- ggplot(all_zones %>% filter(zone == "Low severity"), aes(x = area, y = after_stat(count / sum(count)), fill = "forestgreen")) +
  geom_histogram(bins = 20) +
  scale_x_log10(breaks= c(0.01,.1,1,10,100,1000), limits = c(0.01, 10000), labels = label_number(), expand = c(0,0)) +
  scale_fill_manual(guide = 'none', values = c("dodgerblue"))+
  labs(y= "", x = "") + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(c(0, 0.8))

a + b + c

ggplot(all_zones, aes(x = area, y = after_stat(count / sum(count)), fill = color)) +
  geom_histogram(bins = 20) +
  scale_x_log10(breaks= c(0.01,.1,1,10,100,1000), limits = c(0.1, 10000), labels = label_number(), expand = c(0,0)) +
  scale_fill_manual(name = "",labels = c("Vegetated patches", "Canopy gaps"), values = c("forestgreen", "dodgerblue"))+
  labs(y= "", x = "") + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(c(0, 0.8))


# Get 3d plots from lidar of severity levels ------------------------------

fully_mtnash <- coverage 
fully_mtnash[fully_mtnash != 25] <- NA
plot(fully_mtnash)


canopy_change_masked <- mask(canopy_change, fully_mtnash)
plot(canopy_change_masked)

sample_canopy <- as.data.frame(canopy_change_masked, xy = TRUE)
names(sample_canopy) <- c("x", "y", "canopy_change")
sample_canopy <- sample_canopy %>%
  mutate(canopy_change = ifelse(canopy_change >= 0, 0, -1*canopy_change),
         canopy_change = as.factor(round(canopy_change, digits = 1))) %>%
  group_by(canopy_change) %>%
  slice_sample(n=10)
sample_canopy$ID <- rep(1:10, 11)
sample_canopy$groupID <- rep(0:10, each = 10)
sample_canopy$row <- row_number(sample_canopy)
sample_canopy <- sample_canopy %>%
  mutate(uniqueID = paste0("plot", groupID, "_", ID))


sample_canopy_rast <- rast(sample_canopy[,c(1,2,6)], type ="xyz")
crs(sample_canopy_rast) <- crs(rast2007)
sample_canopy_poly <- as.polygons(sample_canopy_rast, values = TRUE)
sample_canopy_poly <- st_as_sf(sample_canopy_poly)
sample_canopy_poly <- full_join(sample_canopy_poly, sample_canopy)



write_sf(sample_canopy_poly, "Outputs/sample_canopy_poly.shp", overwrite = TRUE)






unburnt3s <- df %>%
  filter(transition ==3 & CLASS == NA)
