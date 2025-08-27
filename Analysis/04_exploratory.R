library(factoextra)
library(corrplot)
library(tidyverse)


#### 2007 ####
#Read the metrics data frame
dframe <- read.csv("Data/training_data2007_full_filt.csv")

plot2007 <- dframe %>%
  dplyr::select(-x,-y,-plotID)

plot2007 <- as.data.frame(scale(select_if(plot2007, is.numeric)))
plot2007$str2007 <- as.factor(dframe$str2007)

table(plot2007$str2007)


# Boxplots ----------------------------------------------------------------

ggplot(plot2007, aes(x = str2007, y = canopy_het)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = height50)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = height95)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = height99)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = sdheigt)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = sdcanopy)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = IQR)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = kurt)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = skew)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2007, aes(x = str2007, y = canopy_cover)) +
  geom_boxplot()+
  geom_jitter()


#Variable combos:
#Height 95/99, sdheight, sdcanopy
#canopy_het, height 50, IQR, skew, canopy_cover very different
#variables:
variables <- c(1,2,7,9,10)

# Correlation -------------------------------------------------------------

corMat<-cor(plot2007[, variables], use="complete.obs", method = "pearson")

corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")

variables1 <- c(1,2,7,9)
variables2 <- c(2,7,9,10)
variables3 <- c(1,2,7)
variables4 <- c(2,7,10)
# PCA ---------------------------------------------------------------------

trial_pca <- prcomp(plot2007[,variables1], scale=TRUE)
fviz_pca_biplot(trial_pca, label="var", habillage = plot2007$str2007)

trial_pca <- prcomp(plot2007[,variables2], scale=TRUE)
fviz_pca_biplot(trial_pca, label="var", habillage = plot2007$str2007)

trial_pca <- prcomp(plot2007[,variables3], scale=TRUE)
fviz_pca_biplot(trial_pca, label="var", habillage = plot2007$str2007)

trial_pca <- prcomp(plot2007[,variables4], scale=TRUE)
fviz_pca_biplot(trial_pca, label="var", habillage = plot2007$str2007)





#### 2016 ####
#Read the metrics data frame
dframe <- read.csv("Data/training_data2016_full_filt.csv")
dframe <- na.omit(dframe)

plot2016 <- dframe %>%
  dplyr::select(-x,-y,-plotID)

plot2016 <- as.data.frame(scale(select_if(plot2016, is.numeric)))
plot2016$str2016 <- as.factor(dframe$str2016)


# Boxplots ----------------------------------------------------------------

ggplot(plot2016, aes(x = str2016, y = canopy_het)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = height50)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = height95)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = height99)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = sdheigt)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = sdcanopy)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = IQR)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = kurt)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = skew)) +
  geom_boxplot()+
  geom_jitter()

ggplot(plot2016, aes(x = str2016, y = canopy_cover)) +
  geom_boxplot()+
  geom_jitter()


names(plot2016)
#Variable combos unclear
variables <- c(3, 6,7,8,9,10)


# Correlation -------------------------------------------------------------

corMat<-cor(plot2016[, variables], use="complete.obs", method = "pearson")

corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")

variables3 <- c(1,2,7)
variables4 <- c(2,7,10)

trial_pca <- prcomp(plot2016[,variables3], scale=TRUE)
fviz_pca_biplot(trial_pca, label="var", habillage = plot2016$str2016)

trial_pca <- prcomp(plot2016[,variables4], scale=TRUE)
fviz_pca_biplot(trial_pca, label="var", habillage = plot2016$str2016)

