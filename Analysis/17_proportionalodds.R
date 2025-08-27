library("tidyverse")
library("brms")
library("bayesplot")
library("data.table")
library("caret")
library("corrplot")
library("terra")
library("sf")



set.seed(406)

# Remove logged 4ha plots -------------------------------------------------
plots <- vect("Data/plot_level.shp")

log_rast <- rast("Spatial/log_year.grd")
logged <- log_rast
logged[logged < 1977] <- NA


check <- terra::extract(logged, plots, ID = TRUE)
check <- check %>%
  filter(!is.na(year))
check <- distinct(check, .keep_all = TRUE)
remove <- unique(check$ID)


# Data setup --------------------------------------------------------------

plots <- read.csv("Data/plot_level.csv") %>%
  select(transitioned, severe, mtnash, partial2007, mean.elevation, mean.slope, mean.east, mean.north, parks, TWI, NA.)
names(plots) <- c(names(plots)[1:10], "ID")

buffer400 <- read.csv("Data/buffer400.csv") %>% select(!mtnash)
buffer600 <- read.csv("Data/buffer600.csv") %>% select(!mtnash)
buffer800 <- read.csv("Data/buffer800.csv") %>% select(!mtnash)
buffer1000 <- read.csv("Data/buffer1000.csv") %>% select(!mtnash)

harv15_40_400 <- read.csv("Data/buffer400_harv15_40.csv")
harv15_40_600 <- read.csv("Data/buffer600_harv15_40.csv")
harv15_40_800 <- read.csv("Data/buffer800_harv15_40.csv")
harv15_40_1000 <- read.csv("Data/buffer1000_harv15_40.csv")

buffer400$harv15_40 <- harv15_40_400$count
buffer600$harv15_40 <- harv15_40_600$count
buffer800$harv15_40 <- harv15_40_800$count
buffer1000$harv15_40 <- harv15_40_1000$count

buffer400 <- cbind(plots[,1:3], buffer400)
buffer600 <- cbind(plots[,1:3], buffer600)
buffer800 <- cbind(plots[,1:3], buffer800)
buffer1000 <- cbind(plots[,1:3], buffer1000)

buffer400 <- buffer400 %>%
  mutate(wet = wet-mtnash)
buffer600 <- buffer600 %>%
  mutate(wet = wet-mtnash)
buffer800 <- buffer800 %>%
  mutate(wet = wet-mtnash)
buffer1000 <- buffer1000 %>%
  mutate(wet = wet-mtnash)


corMat<-cor(plots[, c("mean.elevation","mean.slope","mean.east", "mean.north", "TWI", "parks")], use="complete.obs", method = "pearson")
corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")



corMat <- cor(buffer400[,c("cov_elev", "cov_slope", "parks", "mean_tslf", "min_tslf",  "wet", "dry", "rainfores")],use="complete.obs", method = "pearson")
corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")


corMat <- cor(buffer600[,c("cov_elev", "cov_slope", "parks", "mean_tslf", "min_tslf",  "wet", "dry", "rainfores")],use="complete.obs", method = "pearson")
corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")


corMat <- cor(buffer800[,c("cov_elev", "cov_slope", "parks", "mean_tslf", "min_tslf",  "wet", "dry", "rainfores")],use="complete.obs", method = "pearson")
corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")

corMat <- cor(buffer1000[,c("cov_elev", "cov_slope", "parks", "mean_tslf", "min_tslf",  "wet", "dry", "rainfores")],use="complete.obs", method = "pearson")
corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")

corMat <- cor(buffer1000[,c("clearfell", "STS", "cov_elev", "cov_slope", "parks", "mean_tslf", "min_tslf",  "wet", "dry", "rainfores")],use="complete.obs", method = "pearson")
corrplot(corMat, 
         method="shade",
         type="lower",
         diag = FALSE,
         addCoef.col = "black")

# plot level predictors ---------------------------------------------------


plots <- plots %>%
  mutate(across(!c("transitioned", "severe", "mtnash", "ID"),~c(scale(.)))) %>%
  filter(transitioned > 0 & !(ID %in% remove))


# train <- createDataPartition(plots$transitioned,
#                              time=1,#the number of partitions to create
#                              p = 0.7,#he percentage of data that goes to training
#                              list=F)


data_train <- plots#[train,]

#data_test <- plots[-train,]


# Set prior  
my_priors <- c(brms::set_prior("normal(0, 1)", class = "b")) 
# Set formula
my_formula <- brms::brmsformula(formula =  severe | trials(mtnash) ~ 1 +  mean.elevation+ mean.slope+  mean.east+ mean.north+ TWI +parks, family = binomial()) 

# Fit model
brm_fit <- brms::brm(my_formula, 
                     family = brms::binomial(),
                     data = data_train,
                     prior = my_priors, 
                     init = "0", 
                     chains = 2, 
                     warmup = 2000, 
                     iter = 4000, 
                     refresh = 0,
                     control = list(adapt_delta = 0.9, max_treedepth = 10)
)

saveRDS(brm_fit, "Outputs/models/plot_level_global.rds")
# Model summary
summary(brm_fit, digits = 3)
mcmc_areas(brm_fit, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 

# 400m buffer -------------------------------------------------------------

buffer400 <- buffer400 %>%
  select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, harv15_40, clearfell, mean_tslf, min_tslf, wet, dry, rainfores, riparian, otherforest, woodheath, nonveg, ID) %>%
  mutate(other = rowSums(across(c(otherforest, woodheath, nonveg))),
         total = rowSums(across(c(wet, dry, rainfores, riparian, otherforest, woodheath, nonveg)))) %>%
  mutate(across(!c("transitioned", "severe", "mtnash", "ID"),~c(scale(.)))) %>%
  filter(transitioned > 0 & !(ID %in% remove))

data_train <- buffer400#[train,]

#data_test <- buffer400[-train,]





# Set prior  
my_priors <- c(brms::set_prior("normal(0, 1)", class = "b")) 
# Set formula
my_formula <- brms::brmsformula(formula =  transitioned| trials(mtnash) ~ 1 + harv15_40 + cov_elev+ cov_slope+ parks+  mean_tslf+ min_tslf+ wet+ dry+ rainfores, family = binomial()) 

# Fit model
brm_fit <- brms::brm(my_formula, 
                     family = brms::binomial(),
                     data = data_train,
                     prior = my_priors, 
                     init = "0", 
                     chains = 2, 
                     warmup = 2000, 
                     iter = 4000, 
                     refresh = 0,
                     control = list(adapt_delta = 0.9, max_treedepth = 10)
)
saveRDS(brm_fit, "Outputs/models/buffer400_global.rds")
# Model summary
summary(brm_fit, digits = 3)
mcmc_areas(brm_fit, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 





# 600m buffer -------------------------------------------------------------


buffer600 <- buffer600 %>%
  select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, harv15_40, clearfell, mean_tslf, min_tslf, wet, dry, rainfores, riparian, otherforest, woodheath, nonveg, ID) %>%
  mutate(across(!c("transitioned", "severe", "mtnash", "ID"),~c(scale(.)))) %>%
  filter(transitioned > 0 & !(ID %in% remove))

data_train <- buffer600#[train,]

#data_test <- buffer600[-train,]





# Set prior  
my_priors <- c(brms::set_prior("normal(0, 1)", class = "b")) 
# Set formula
my_formula <- brms::brmsformula(formula =  transitioned| trials(mtnash) ~ 1 + harv15_40 + cov_elev+ cov_slope+ parks+  mean_tslf+ min_tslf+ wet+ dry+ rainfores, family = binomial()) 
# Fit model
brm_fit <- brms::brm(my_formula, 
                     family = brms::binomial(),
                     data = data_train,
                     prior = my_priors, 
                     init = "0", 
                     chains = 2, 
                     warmup = 2000, 
                     iter = 4000, 
                     refresh = 0,
                     control = list(adapt_delta = 0.9, max_treedepth = 10)
)
saveRDS(brm_fit, "Outputs/models/buffer600_global.rds")
# Model summary
summary(brm_fit, digits = 3)
mcmc_areas(brm_fit, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 





# 800 m buffer ------------------------------------------------------------


buffer800 <- buffer800 %>%
  select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, harv15_40, clearfell, mean_tslf, min_tslf, wet, dry, rainfores, riparian, otherforest, woodheath, nonveg, ID) %>%
  mutate(across(!c("transitioned", "severe", "mtnash", "ID"),~c(scale(.)))) %>%
  filter(transitioned > 0 & !(ID %in% remove))

data_train <- buffer800#[train,]

#data_test <- buffer800[-train,]





# Set prior  
my_priors <- c(brms::set_prior("normal(0, 1)", class = "b")) 
# Set formula
my_formula <- brms::brmsformula(formula =  transitioned| trials(mtnash) ~ 1 + harv15_40 + cov_elev+ cov_slope+ parks+  mean_tslf+ min_tslf+ wet+ dry+ rainfores, family = binomial()) 
# Fit model
brm_fit <- brms::brm(my_formula, 
                     family = brms::binomial(),
                     data = data_train,
                     prior = my_priors, 
                     init = "0", 
                     chains = 2, 
                     warmup = 2000, 
                     iter = 4000, 
                     refresh = 0,
                     control = list(adapt_delta = 0.9, max_treedepth = 10)
)
saveRDS(brm_fit, "Outputs/models/buffer800_global.rds")
# Model summary
summary(brm_fit, digits = 3)
mcmc_areas(brm_fit, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 



# 1000 m buffer ------------------------------------------------------------


buffer1000 <- buffer1000 %>%
  #select(transitioned, severe, mtnash, partial2007, cov_elev, cov_slope, parks, area_logged, log_10, log_20, log_30, log_40, log_50, log_60, log_70, clearfell, STS, thinning,  mean_tslf, min_tslf, wet, dry, rainfores, riparian, otherforest, woodheath, nonveg) %>%
  mutate(across(!c("transitioned", "severe", "mtnash", "ID", "dom_ten"),~c(scale(.)))) %>%
  filter(transitioned > 0 & !(ID %in% remove))

data_train <- buffer1000#[train,]

#data_test <- buffer1000[-train,]





# Set prior  
my_priors <- c(brms::set_prior("normal(0, 1)", class = "b")) 
# Set formula
my_formula <- brms::brmsformula(formula =  transitioned| trials(mtnash) ~ 1 + harv15_40 + cov_elev+ cov_slope+ parks+  mean_tslf+ min_tslf+ wet+ dry+ rainfores, family = binomial()) 
# Fit model
brm_fit <- brms::brm(my_formula, 
                     family = brms::binomial(),
                     data = data_train,
                     prior = my_priors, 
                     init = "0", 
                     chains = 2, 
                     warmup = 2000, 
                     iter = 4000, 
                     refresh = 0,
                     control = list(adapt_delta = 0.9, max_treedepth = 10)
)
saveRDS(brm_fit, "Outputs/models/buffer1000_global.rds")
# Model summary
summary(brm_fit, digits = 3)
mcmc_areas(brm_fit, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 





# 1000 m buffer harvesting analysis  ------------------------------------


# Set prior  
my_priors <- c(brms::set_prior("normal(0, 1)", class = "b")) 
# Set formula
my_formula <- brms::brmsformula(formula =  transitioned| trials(mtnash) ~ 1 + harv15_40 + cov_elev+ cov_slope+ parks+  mean_tslf+ min_tslf+ wet+ dry+ rainfores, family = binomial()) 
# Fit model
brm_fit <- brms::brm(my_formula, 
                     family = brms::binomial(),
                     data = data_train,
                     prior = my_priors, 
                     init = "0", 
                     chains = 2, 
                     warmup = 2000, 
                     iter = 4000, 
                     refresh = 0,
                     control = list(adapt_delta = 0.9, max_treedepth = 10)
)
saveRDS(brm_fit, "Outputs/models/hbuffer1000_log70.rds")
# Model summary
summary(brm_fit, digits = 3)
mcmc_areas(brm_fit, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 


# 1000 m buffer harvesting analysis 15-40 years ------------------------------------


# Set prior  
my_priors <- c(brms::set_prior("normal(0, 1)", class = "b")) 
# Set formula
my_formula <- brms::brmsformula(formula =  transitioned| trials(mtnash) ~ 1 + harv15_40 + cov_elev+ cov_slope+ parks+  mean_tslf+ min_tslf+ wet+ dry+ rainfores, family = binomial()) 
# Fit model
brm_fit <- brms::brm(my_formula, 
                     family = brms::binomial(),
                     data = data_train,
                     prior = my_priors, 
                     init = "0", 
                     chains = 2, 
                     warmup = 2000, 
                     iter = 4000, 
                     refresh = 0,
                     control = list(adapt_delta = 0.9, max_treedepth = 10)
)
saveRDS(brm_fit, "Outputs/models/hbuffer1000_harv15_40.rds")
# Model summary
summary(brm_fit, digits = 3)
mcmc_areas(brm_fit, regex_pars = 'b_') + geom_vline(xintercept = 0) + theme_bw() 
