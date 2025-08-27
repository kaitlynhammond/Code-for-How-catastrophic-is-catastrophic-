library("brms")
library("bayesplot")
library("caret")
library("tidyverse")



set.seed(406)


mod1 <- readRDS("Outputs/models/buffer400_3.rds")
mod2 <- readRDS("Outputs/models/buffer400_2.rds")
mod3 <- readRDS("Outputs/models/buffer600_2.rds")
mod4 <- readRDS("Outputs/models/buffer600_4.rds")
mod5 <- readRDS("Outputs/models/buffer800_1.rds")
mod6 <- readRDS("Outputs/models/buffer800_4.rds")
mod7 <- readRDS("Outputs/models/buffer1000_2.rds")
mod8 <- readRDS("Outputs/models/buffer1000_1.rds")
mod9 <- readRDS("Outputs/models/plot_level_2.rds")
mod10 <- readRDS("Outputs/models/plot_level_1.rds")
mod11 <- readRDS("Outputs/mixed_models/mixedmodels_5.rds")
mod12 <- readRDS("Outputs/mixed_models/mixedmodels_4.rds")


# Data setup --------------------------------------------------------------

plots <- read.csv("Data/plot_level.csv") %>%
  select(transitioned, severe, mtnash, partial2007, mean.elevation, mean.slope, mean.east, mean.north, parks, TWI)

buffer400 <- read.csv("Data/buffer400.csv") %>% select(!mtnash)
buffer600 <- read.csv("Data/buffer600.csv") %>% select(!mtnash)
buffer800 <- read.csv("Data/buffer800.csv") %>% select(!mtnash)
buffer1000 <- read.csv("Data/buffer1000.csv") %>% select(!mtnash)

buffer400 <- cbind(plots[,1:3], buffer400)
buffer600 <- cbind(plots[,1:3], buffer600)
buffer800 <- cbind(plots[,1:3], buffer800)
buffer1000 <- cbind(plots[,1:3], buffer1000)



plots <- plots %>%
  mutate(across(!c("transitioned", "severe", "mtnash"),~c(scale(.)))) %>%
  filter(mtnash > 0)


train <- createDataPartition(plots$transitioned,
                             time=1,#the number of partitions to create
                             p = 0.7,#he percentage of data that goes to training
                             list=F)


data_train <- plots[train,]

data_test <- plots[-train,]


actual <- data_test$transitioned


# Mods 1 and 2 ------------------------------------------------------------
buffer400 <- buffer400 %>%
  select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, clearfell, mean_tslf, min_tslf, wet, dry, rainrip, otherforest, woodheath, nonveg) %>%
  mutate(across(!c("transitioned", "severe", "mtnash"),~c(scale(.)))) %>%
  filter(mtnash > 0)

data_test <- buffer400[-train,]


pred1 <- as.data.frame(predict(mod1, data_test))
pred2 <- as.data.frame(predict(mod2, data_test))



# Mods 3 and 4 ------------------------------------------------------------


buffer600 <- buffer600 %>%
  select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, clearfell, mean_tslf, min_tslf, wet, dry, rainrip, otherforest, woodheath, nonveg) %>%
  mutate(across(!c("transitioned", "severe", "mtnash"),~c(scale(.)))) %>%
  filter(mtnash > 0)

data_test <- buffer600[-train,]


pred3 <- as.data.frame(predict(mod3, data_test))
pred4 <- as.data.frame(predict(mod4, data_test))




# Mods 5 and 6 ------------------------------------------------------------

buffer800 <- buffer800 %>%
  select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, clearfell, mean_tslf, min_tslf, wet, dry, rainrip, otherforest, woodheath, nonveg) %>%
  mutate(across(!c("transitioned", "severe", "mtnash"),~c(scale(.)))) %>%
  filter(mtnash > 0)

data_test <- buffer800[-train,]


pred5 <- as.data.frame(predict(mod5, data_test))
pred6 <- as.data.frame(predict(mod6, data_test))




# Mods 7 and 8 ------------------------------------------------------------

buffer1000 <- buffer1000 %>%
  select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, clearfell, mean_tslf, min_tslf, wet, dry, rainrip, otherforest, woodheath, nonveg) %>%
  mutate(across(!c("transitioned", "severe", "mtnash"),~c(scale(.)))) %>%
  filter(mtnash > 0)

data_test <- buffer1000[-train,]


pred7 <- as.data.frame(predict(mod7, data_test))
pred8 <- as.data.frame(predict(mod8, data_test))




# Mods 9 and 10 -----------------------------------------------------------

data_test <- plots[-train,]


pred9 <- as.data.frame(predict(mod9, data_test))
pred10 <- as.data.frame(predict(mod10, data_test))




# Mixed modesl ------------------------------------------------------------



mixed_df <- cbind(plots[,c(1:3,5,6,7,8,10)], buffer800$cov_elev, buffer600$cov_slope, buffer600$dry, buffer1000$mean_tslf, buffer600$min_tslf, buffer1000$parks, buffer1000$partial2007, buffer400$rainrip, buffer800$woodheath)
names(mixed_df) <- c(names(mixed_df[,1:8]), "cov_elev", "cov_slope", "dry", "mean_tslf", "min_tslf", "parks", "partial2007", "rainrip", "woodheath")



data_test <- mixed_df[-train,]



pred11 <- as.data.frame(predict(mod11, data_test))
pred12 <- as.data.frame(predict(mod12, data_test))



# Predictions -------------------------------------------------------------

pred1 <- as.data.frame(cbind(actual, pred1))
pred2 <- as.data.frame(cbind(actual, pred2))
pred3 <- as.data.frame(cbind(actual, pred3))
pred4 <- as.data.frame(cbind(actual, pred4))
pred5 <- as.data.frame(cbind(actual, pred5))
pred6 <- as.data.frame(cbind(actual, pred6))
pred7 <- as.data.frame(cbind(actual, pred7))
pred8 <- as.data.frame(cbind(actual, pred8))
pred9 <- as.data.frame(cbind(actual, pred9))
pred10 <- as.data.frame(cbind(actual, pred10))
pred11 <- as.data.frame(cbind(actual, pred11))
pred12 <- as.data.frame(cbind(actual, pred12))




ggplot(pred1) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred2) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred3) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred4) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred5) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred6) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred7) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred8) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred9) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred10) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred11) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")

ggplot(pred12) +
  geom_pointrange(aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = actual)) +
  geom_abline (slope=1, linetype = "dashed", color="Red")



mcmc_areas(mod1, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod2, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod3, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod4, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod5, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod6, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod7, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod8, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod9, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod10, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod11, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
mcmc_areas(mod12, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
