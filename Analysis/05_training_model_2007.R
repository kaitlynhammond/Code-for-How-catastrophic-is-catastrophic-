library(caret)
library(corrplot)
library(tidyverse)


set.seed(406)


all_data2007 <- read.csv("Data/filtered_2007mets_22_02_2024.csv")
dframe2007 <- read.csv("Data/training_data2007_full_filt.csv")

all_data2007$training <- "No"
dframe2007$training <- "yes"


combined2007 <- full_join(all_data2007, dframe2007)

combined2007 <- combined2007 %>%
  mutate(across(canopy_het:canopy_cover, ~(scale(.) %>% as.vector)))


corMat<-cor(combined2007[, c(3,4,5,7,8,10,12)], use="complete.obs", method = "pearson")

corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")

# all_scaled <- combined2007 %>%
#   dplyr::filter(training == "No") %>%
#   dplyr::select(-training, -plotID, -str2007)
# 
# training_scaled <- combined2007 %>%
#   dplyr::filter(training == "yes") %>%
#   dplyr::select(-CLASS, -training)
# 
# apply(combined[,3:12], 2, sd)


# write.csv(all_scaled, "Data/scaled_2007mets_22_02_2024.csv", row.names = FALSE)
# write.csv(training_scaled, "Data/training_data2007_full_filt_scaled.csv", row.names = FALSE)


# 2007 binomial model with balanced sample --------------------------------
dframe <- read.csv("Data/training_data2007_full_filt_scaled.csv") %>%
  mutate(class = as.factor(case_when(str2007 == "1" ~ 0,
                                     str2007 == "2" ~ 1)),
         str2007 = as.factor(str2007)) %>%
group_by(str2007) %>%
slice_sample(n=162) %>%
  ungroup()


dframe <- dframe %>%
  dplyr::select(-x,-y,-plotID)

train <- createDataPartition(dframe$class,
                             time=1,#the number of partitions to create
                             p = 0.7,#he percentage of data that goes to training
                             list=F)

data_train2 <- dframe[train,]
data_test2<- dframe[-train,]
  






mod1 <- glm(class ~ canopy_het + height50 + height95 + sdheigt + sdcanopy + IQR + skew,
            family = binomial(link = "logit"),
            data=data_train2)
summary(mod1)
confint(mod1)

data_test2$mod1 <- predict(mod1, data_test2, type = "response")
data_test2 <- data_test2 %>%
  mutate(mod1 = as.factor(case_when(mod1 < 0.5 ~ 0,
                                    mod1 >=  0.5 ~ 1)))

cf1 <- confusionMatrix(data_test2$class, data_test2$mod1)
cf1




mod2 <- glm(class ~ canopy_cover + height50 + height95 + sdheigt + sdcanopy + IQR + skew,
            family = binomial(link = "logit"),
            data=data_train2, na.action='na.fail')

summary(mod2)
confint(mod2)

data_test2$mod2 <- predict(mod2, data_test2, type = "response")
data_test2 <- data_test2 %>%
  mutate(mod2 = as.factor(case_when(mod2 < 0.5 ~ 0,
                                    mod2 >=  0.5 ~ 1)))

cf2 <- confusionMatrix(data_test2$class, data_test2$mod2)
cf2




mod3 <- glm(class ~ canopy_cover + height50 + sdheigt,
            family = binomial(link = "logit"),
            data=data_train2, na.action='na.fail')

summary(mod3)
confint(mod3)

data_test2$mod3 <- predict(mod3, data_test2, type = "response")
data_test2 <- data_test2 %>%
  mutate(mod3 = as.factor(case_when(mod3 < 0.5 ~ 0,
                                    mod3 >=  0.5 ~ 1)))

cf3 <- confusionMatrix(data_test2$class, data_test2$mod3)
cf3




mod4 <- glm(class ~ canopy_het + height50 +sdheigt,
            family = binomial(link = "logit"),
            data=data_train2, na.action='na.fail')

summary(mod4)
confint(mod4)

data_test2$mod4 <- predict(mod4, data_test2, type = "response")
data_test2 <- data_test2 %>%
  mutate(mod4 = as.factor(case_when(mod4 < 0.5 ~ 0,
                                    mod4 >=  0.5 ~ 1)))

cf4 <- confusionMatrix(data_test2$class, data_test2$mod4)
cf4


mod5 <- glm(class ~ canopy_cover + skew,
            family = binomial(link = "logit"),
            data=data_train2, na.action='na.fail')

summary(mod5)
confint(mod5)

data_test2$mod5 <- predict(mod5, data_test2, type = "response")
data_test2 <- data_test2 %>%
  mutate(mod5 = as.factor(case_when(mod5 < 0.5 ~ 0,
                                    mod5 >=  0.5 ~ 1)))

cf5 <- confusionMatrix(data_test2$class, data_test2$mod5)
cf5


# Make a nice table -------------------------------------------------------

all_cf <- list(cf1, cf2, cf3, cf4, cf5)
all_mods <- list(mod1, mod2, mod3, mod4, mod5)


variables <- lapply(all_mods, function(x) attr(x$terms, "term.labels"))
variables <- sapply(variables, function (x) paste(x, collapse = " "))

AICs <- sapply(all_mods, function(x) x$aic)
accuracy <- sapply(all_cf, function(x) x$overall["Accuracy"])
kappa <- sapply(all_cf, function(x) x$overall["Kappa"])

sens <- sapply(all_cf, function(x) x$byClass["Sensitivity"])
spec <- sapply(all_cf, function(x) x$byClass["Specificity"])
bal <- sapply(all_cf, function(x) x$byClass["Balanced Accuracy"])
names <- c("mod1", "mod2", "mod3", "mod4", "mod5")
rows <- c("AIC", "Accuracy", "Kappa","Sensitivity", "Specificity", "Balanced Accuracy")


combined <- data.frame(rbind(AICs, accuracy, kappa, sens, spec, bal))
names(combined) <- names
row.names(combined) <- rows
combined <- format(round(combined, 2), nsmall = 2)


combined <- rbind(variables, combined)

  
saveRDS(mod4, "Outputs/binomial_2007.rds")
