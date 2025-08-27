library(caret)
library(MASS)
library(nnet)
library(corrplot)
library(tidyverse)

set.seed(406)

# #Scaling and correlation

all_data2016 <- read.csv("Data/filtered_2016mets_22_02_2024.csv")
dframe2016 <- read.csv("Data/training_data2016_full_filt.csv")

all_data2016$training <- "No"
dframe2016$training <- "yes"


combined2016 <- full_join(all_data2016, dframe2016)

combined2016 <- combined2016 %>%
  mutate(across(canopy_het:canopy_cover, ~(scale(.) %>% as.vector)))


corMat<-cor(combined2016[, c(3,5,8,9,10,11,12)], use="complete.obs", method = "pearson")

corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")

# all_scaled <- combined2016 %>%
#   dplyr::filter(training == "No") %>%
#   dplyr::select(-training, -plotID, -str2016)
# 
# training_scaled <- combined2016 %>%
#   dplyr::filter(training == "yes") %>%
#   dplyr::select(-CLASS, -training)
# 
# apply(combined2016[,3:12], 2, sd)


# write.csv(all_scaled, "Data/scaled_2016mets_22_02_2024.csv", row.names = FALSE)
# write.csv(training_scaled, "Data/training_data2016_full_filt_scaled.csv", row.names = FALSE)










#Notes:
#Correlated pairs: 
#canopy_cover and canopy_het
#IQR and sdheigt
#sdheigt and height99/height95
#Kurtosis not well represented in training set




# Ordered probit regression 2016 ------------------------------------------
rm(list = ls())
dframe <- read.csv("Data/training_data2016_full_filt_scaled.csv") %>% 
  mutate(str2016 = as.factor(str2016)) %>%
  group_by(str2016) %>%
  slice_sample(n=212) %>%
  ungroup()

dframe <- dframe %>%
  dplyr::select(-x,-y,-plotID)


train <- createDataPartition(dframe$str2016,
                             time=1,#the number of partitions to create
                             p = 0.7,#he percentage of data that goes to training
                             list=F)

data_train1 <- dframe[train,]
data_test1<- dframe[-train,]



mod1 <- polr(str2016 ~ canopy_het + height95 + sdcanopy + IQR +  skew,
            data=data_train1, na.action='na.fail',
            Hess=TRUE, method = "logistic")

summary(mod1)
confint(mod1)

data_test1$mod1 <- predict(mod1, data_test1, type = "class")

cf1 <- confusionMatrix(data_test1$str2016, data_test1$mod1)
cf1




mod2 <- polr(str2016 ~ canopy_cover + height95 + sdcanopy + IQR +  skew, 
             data=data_train1, na.action='na.fail',
             Hess=TRUE, method = "logistic")
summary(mod2)
confint(mod2)

data_test1$mod2 <- predict(mod2, data_test1, type = "class")

cf2 <- confusionMatrix(data_test1$str2016, data_test1$mod2)
cf2



mod3 <- polr(str2016 ~ canopy_het + height95 + IQR +  skew,
             data=data_train1, na.action='na.fail',
             Hess=TRUE, method = "logistic")
summary(mod3)
confint(mod3)

data_test1$mod3 <- predict(mod3, data_test1, type = "class")

cf3 <- confusionMatrix(data_test1$str2016, data_test1$mod3)
cf3



mod4 <- polr(str2016 ~ canopy_cover + height95 + IQR +  skew,
             data=data_train1, na.action='na.fail',
             Hess=TRUE, method = "logistic")
summary(mod4)
confint(mod4)

data_test1$mod4 <- predict(mod4, data_test1, type = "class")

cf4 <- confusionMatrix(data_test1$str2016, data_test1$mod4)
cf4

# Multinomial model -------------------------------------------------------
mod5 <- multinom(str2016 ~ canopy_het + height95 + sdcanopy + IQR +  skew,
             data=data_train1, na.action='na.fail',
             Hess=TRUE, method = "logistic")
summary(mod5)
confint(mod5)

data_test1$mod5 <- predict(mod5, data_test1, type = "class")

cf5 <- confusionMatrix(data_test1$str2016, data_test1$mod5)
cf5




mod6 <- multinom(str2016 ~ canopy_cover + height95 + sdcanopy + IQR +  skew,
             data=data_train1, na.action='na.fail',
             Hess=TRUE, method = "logistic")
summary(mod6)
confint(mod6)

data_test1$mod6 <- predict(mod6, data_test1, type = "class")

cf6 <- confusionMatrix(data_test1$str2016, data_test1$mod6)
cf6



mod7 <- multinom(str2016 ~ canopy_het + height95 + IQR +  skew,
             data=data_train1, na.action='na.fail',
             Hess=TRUE, method = "logistic")
summary(mod7)
confint(mod7)

data_test1$mod7 <- predict(mod7, data_test1, type = "class")

cf7 <- confusionMatrix(data_test1$str2016, data_test1$mod7)
cf7

mod8 <- multinom(str2016 ~ canopy_cover + height95 + IQR +  skew,
                 data=data_train1, na.action='na.fail',
                 Hess=TRUE, method = "logistic")
summary(mod8)
confint(mod8)

data_test1$mod8 <- predict(mod8, data_test1, type = "class")

cf8 <- confusionMatrix(data_test1$str2016, data_test1$mod8)
cf8





# Make a nice table -------------------------------------------------------

all_cf <- list(cf1, cf2, cf3, cf4,  cf5, cf6, cf7, cf8)
all_mods <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

variables <- lapply(all_mods, function(x) attr(x$terms, "term.labels"))
variables <- sapply(variables, function (x) paste(x, collapse = " "))

AICs <- sapply(all_mods, AIC)
accuracy <- sapply(all_cf, function(x) x$overall["Accuracy"])
kappa <- sapply(all_cf, function(x) x$overall["Kappa"])

sens1 <- sapply(all_cf, function(x) x$byClass["Class: 1", "Sensitivity"])
sens2 <- sapply(all_cf, function(x) x$byClass["Class: 2", "Sensitivity"])
sens3 <- sapply(all_cf, function(x) x$byClass["Class: 3", "Sensitivity"])

spec1 <- sapply(all_cf, function(x) x$byClass["Class: 1", "Specificity"])
spec2 <- sapply(all_cf, function(x) x$byClass["Class: 2", "Specificity"])
spec3 <- sapply(all_cf, function(x) x$byClass["Class: 3", "Specificity"])

bal1 <- sapply(all_cf, function(x) x$byClass["Class: 1", "Balanced Accuracy"])
bal2 <- sapply(all_cf, function(x) x$byClass["Class: 2", "Balanced Accuracy"])
bal3 <- sapply(all_cf, function(x) x$byClass["Class: 3", "Balanced Accuracy"])


names <- c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8")
rows <- c("AIC", "Accuracy", "Kappa",
          "Class 1: Sensitivity", "Class 2: Sensitivity", "Class 3: Sensitivity",
          "Class 1: Specificity", "Class 2: Specificity", "Class 3: Specificity",
          "Class 1: Balanced Accuracy","Class 2: Balanced Accuracy","Class 3: Balanced Accuracy")


combined <- data.frame(rbind(AICs, accuracy, kappa,
                             sens1, sens2, sens3,
                             spec1, spec2, spec3,
                             bal1, bal2, bal3))
names(combined) <- names
row.names(combined) <- rows
combined <- format(round(combined, 2), nsmall = 2)
combined <- rbind(variables, combined)



saveRDS(mod5, "Outputs/multinom_2016.rds")
