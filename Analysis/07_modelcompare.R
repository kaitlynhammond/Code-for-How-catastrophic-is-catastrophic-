library("brms")
library("bayesplot")
library("caret")
library("tidyverse")
library("RColorBrewer")
library(patchwork)


model_files <- list.files("Outputs/models", pattern = "^b", full.names = TRUE)


model_names <- list.files("Outputs/models",  pattern = "^b")
model_names <- as.data.frame(tools::file_path_sans_ext(model_names))
names(model_names) <- "name"
model_names <- separate_wider_delim(model_names, cols = name, names = c("buffer", "ID"), delim = "_")


model_list <- list()


for(i in 1:nrow(model_names)){
  
model_1 <- summary(readRDS(model_files[i]))
mod_table <- model_1[["fixed"]]
mod_table <- mod_table[,c(1,3,4)]


mod_table$buffer <- rep(model_names[i, 1], nrow(mod_table))
mod_table$persistence <- rep(model_names[i, 2], nrow(mod_table))

mod_table <- rownames_to_column(mod_table, "parameter")

model_list[[i]] <- as.data.frame(mod_table)

}


all_parameters <- do.call(rbind, model_list)
names(all_parameters) <- c("parameter", "estimate", "lci", "uci", "buffer", "id")
all_parameters <- as.data.frame(lapply(all_parameters, unlist))
all_parameters$buffer <- factor(all_parameters$buffer, levels = c("buffer400", "buffer600", "buffer800", "buffer1000"))
all_parameters <- all_parameters %>%
  mutate(label = case_when(buffer == "buffer1000" ~ 1000,
                           buffer == "buffer800" ~ 800,
                           buffer == "buffer600" ~ 600,
                           buffer == "buffer400" ~ 400),
         param = case_when(parameter == "cov_elev" ~ "CV of elevation",
                           parameter == "cov_slope" ~ "CV of slope",
                           parameter == "dry" ~ "Dry forest",
                           parameter == "mean_tslf" ~ "Mean TSLF",
                           parameter == "min_tslf" ~ "Minimum TSLF",
                           parameter == "parks" ~ "Parks and Reserves",
                           parameter == "rainfores" ~ "Rainforest",
                           parameter == "wet" ~ "Wet forest",
                           parameter == "harv15_40" ~ "Area harvested"))

ggplot(all_parameters %>% filter(parameter!= "Intercept" & id == "global")) +
  geom_pointrange(aes(x = estimate, xmin = lci, xmax = uci, y = label, color = buffer)) +
  facet_wrap(~factor(param, levels=c("CV of elevation", "CV of slope", "Mean TSLF", "Minimum TSLF", "Area harvested",
                                            "Parks and Reserves", "Wet forest", "Dry forest",
                                            "Rainforest")), nrow = 3) +
  geom_vline(xintercept = 0, linetype='dotted') +
  xlim(-0.8, 0.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        panel.border = element_blank(),
       # panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
       legend.position = "none",,
       axis.line.x = element_line(),
       strip.background = element_rect(
         color="black", fill="white", linewidth = 0.1, linetype="solid"
       )) +
  scale_color_brewer(palette = "Paired")+
  labs(x= "Estimate", y = "Buffer width (m)")
  




# Logging plots -----------------------------------------------------------

model_files <- list.files("Outputs/models", pattern = "^h", full.names = TRUE)


model_names <- list.files("Outputs/models",  pattern = "^hbuffer1000_log")
model_names <- as.data.frame(tools::file_path_sans_ext(model_names))
names(model_names) <- "name"
model_names <- separate_wider_delim(model_names, cols = name, names = c("harv", "persistence"), delim = "_")


model_list <- list()


for(i in 1:nrow(model_names)){
  
  model_1 <- summary(readRDS(model_files[i]))
  mod_table <- model_1[["fixed"]]
  mod_table <- mod_table[,c(1,3,4)]
  
  mod_table$persistence <- rep(model_names[i, 2], nrow(mod_table))
  
  mod_table <- rownames_to_column(mod_table, "parameter")
  
  model_list[[i]] <- as.data.frame(mod_table)
  
}



all_parameters <- do.call(rbind, model_list)
names(all_parameters) <- c("parameter", "estimate", "lci", "uci", "persistence")
all_parameters <- as.data.frame(lapply(all_parameters, unlist))
logging <- all_parameters %>%
  filter(parameter == "log_10" |parameter == "log_20"|parameter == "log_30"|parameter == "log_40"|parameter == "log_50"|parameter == "log_60"|parameter == "log_70") %>%
  mutate(parameter = case_when(parameter == "log_10" ~ 10,
                               parameter == "log_20" ~ 20,
                               parameter == "log_30" ~ 30,
                               parameter == "log_40" ~ 40,
                               parameter == "log_50" ~ 50,
                               parameter == "log_60" ~ 60,
                               parameter == "log_70" ~ 70))


log <- ggplot(logging) +
  geom_pointrange(aes(x = estimate, xmin = lci, xmax = uci, y = factor(parameter))) +
  geom_vline(xintercept = 0) +
  xlim(-0.5, 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        # panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        legend.position = "none",,
        axis.line.x = element_line(),
        strip.background = element_rect(
          color="black", fill="white", linewidth = 0.1, linetype="solid"
        ))+
  labs(x= "Estimate", y = "Persistence time (years)")+
  ggtitle('B)')




# plot level --------------------------------------------------------------

mod1 <- summary(readRDS("Outputs/models/plot_level_global.rds"))

mod_table <- mod1[["fixed"]]
mod_table <- mod_table[,c(1,3,4)]


mod_table$buffer <- rep(model_names[i, 1], nrow(mod_table))

mod_table <- rownames_to_column(mod_table, "parameter")
names(mod_table) <- c("parameter", "estimate", "lci", "uci", "persistence")

mod_table <- mod_table %>%
  mutate(parameter = case_when(parameter == "mean.elevation" ~ "Mean elevation",
                               parameter == "mean.slope" ~ "Mean slope",
                               parameter == "mean.east" ~ "Mean eastness",
                               parameter == "mean.north" ~ "Mean northness",
                               parameter == "TWI" ~ "Topographic wetness index",
                               parameter == "parks" ~ "Parks and reserves",
                               parameter == "Intercept" ~ "Intercept"))



plot <- ggplot(mod_table %>% filter(parameter != "Intercept")) +
  geom_pointrange(aes(x = estimate, xmin = lci, xmax = uci, y = factor(parameter, levels=c("Mean elevation", "Mean slope", "Mean eastness", "Mean northness", "Topographic wetness index", "Parks and reserves")))) +
  geom_vline(xintercept = 0) +
  xlim(-0.5, 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        # panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        legend.position = "none",,
        axis.line.x = element_line(),
        strip.background = element_rect(
          color="black", fill="white", linewidth = 0.1, linetype="solid"
        ))+
  labs(x= "Estimate", y = "")+
  scale_y_discrete(limits=rev)


plot


# LOO for plot measures ---------------------------------------------------

plotg <- readRDS("Outputs/models/plot_level_global.rds")
plot1 <- readRDS("Outputs/models/plot_level_1.rds")
plot2 <- readRDS("Outputs/models/plot_level_2.rds")

loo_g <- loo(plotg)
loo_1 <- loo(plot1)
loo_2 <- loo(plot2)


loo_compare(loo_g, loo_1, loo_2)

#2 is best, then 1

# Calculate LOO -----------------------------------------------------------

buffer400_g <-  readRDS("Outputs/models/buffer400_global.rds")
buffer400_1 <-  readRDS("Outputs/models/buffer400_1.rds")
buffer400_2 <-  readRDS("Outputs/models/buffer400_2.rds")
buffer400_3 <-  readRDS("Outputs/models/buffer400_3.rds")

loo_g <- loo(buffer400_g)
loo_1 <- loo(buffer400_1)
loo_2 <- loo(buffer400_2)
loo_3 <- loo(buffer400_3)

loo_compare(loo_g, loo_1, loo_2, loo_3)

#Model 3 is best, then 2

# LOO buffer 600 ----------------------------------------------------------

buffer600_g <-  readRDS("Outputs/models/buffer600_global.rds")
buffer600_1 <-  readRDS("Outputs/models/buffer600_1.rds")
buffer600_2 <-  readRDS("Outputs/models/buffer600_2.rds")
buffer600_3 <-  readRDS("Outputs/models/buffer600_3.rds")
buffer600_4 <-  readRDS("Outputs/models/buffer600_4.rds")
buffer600_5 <-  readRDS("Outputs/models/buffer600_5.rds")

loo_g <- loo(buffer600_g)
loo_1 <- loo(buffer600_1)
loo_2 <- loo(buffer600_2)
loo_3 <- loo(buffer600_3)
loo_4 <- loo(buffer600_4)
loo_5 <- loo(buffer600_5)

loo_compare(loo_g, loo_1, loo_2, loo_3, loo_4, loo_5)


#Model 2 is best, then 4





# LOO buffer 800 ----------------------------------------------------------

buffer800_g <-  readRDS("Outputs/models/buffer800_global.rds")
buffer800_1 <-  readRDS("Outputs/models/buffer800_1.rds")
buffer800_2 <-  readRDS("Outputs/models/buffer800_2.rds")
buffer800_3 <-  readRDS("Outputs/models/buffer800_3.rds")
buffer800_4 <-  readRDS("Outputs/models/buffer800_4.rds")
buffer800_5 <-  readRDS("Outputs/models/buffer800_5.rds")


loo_g <- loo(buffer800_g)
loo_1 <- loo(buffer800_1)
loo_2 <- loo(buffer800_2)
loo_3 <- loo(buffer800_3)
loo_4 <- loo(buffer800_4)
loo_5 <- loo(buffer800_5)

loo_compare(loo_g, loo_1, loo_2, loo_3, loo_4, loo_5)


#Model 1, then 4



# LOO buffer 1000 ---------------------------------------------------------

buffer1000_g <-  readRDS("Outputs/models/buffer1000_global.rds")
buffer1000_1 <-  readRDS("Outputs/models/buffer1000_1.rds")
buffer1000_2 <-  readRDS("Outputs/models/buffer1000_2.rds")
buffer1000_3 <-  readRDS("Outputs/models/buffer1000_3.rds")

loo_g <- loo(buffer1000_g)
loo_1 <- loo(buffer1000_1)
loo_2 <- loo(buffer1000_2)
loo_3 <- loo(buffer1000_3)

loo_compare(loo_g, loo_1, loo_2, loo_3)



#Model 2, then model 1




# mixed models ------------------------------------------------------------


mixed_1 <-  readRDS("Outputs/mixed_models/mixedmodels_1.rds")
mixed_2 <-  readRDS("Outputs/mixed_models/mixedmodels_2.rds")
mixed_3 <-  readRDS("Outputs/mixed_models/mixedmodels_3.rds")
mixed_4 <-  readRDS("Outputs/mixed_models/mixedmodels_4.rds")
mixed_5 <-  readRDS("Outputs/mixed_models/mixedmodels_5.rds")
mixed_6 <-  readRDS("Outputs/mixed_models/mixedmodels_6.rds")


loo_1 <- loo(mixed_1)
loo_2 <- loo(mixed_2)
loo_3 <- loo(mixed_3)
loo_4 <- loo(mixed_4)
loo_5 <- loo(mixed_5)
loo_6 <- loo(mixed_6)


loo_compare(loo_1, loo_2, loo_3, loo_4, loo_5, loo_6)

#Best is 5 and 4
