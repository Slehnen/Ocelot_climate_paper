library(spocc)
library(rlang)
library(caret)
library(caretEnsemble)
library(gbm)
library(dplyr)
library(foreach)
library(doParallel)
library(sp)
library(rgdal)
library(raster)
library(BiodiversityR)
library(ROCR)
library(rgeos)
library(enmSdm)
library(sf)
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(forcats)
library(fastshap)  # for fast (approximate) Shapley values
library(ranger)    # for fast random forest algorithm

set.seed(123)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Source")
aoi <- readOGR(dsn=getwd(), layer="FWS_10_j_evaluation_AOI")
polys <- st_read("FWS_10_j_evaluation_AOI.shp")
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Source")
# Code below randomly selected three patches from protected areas
# database that were larger than 150,000 ha
#props_sf <- st_read("PAD_AOI.shp")
#props_sf <- subset(props_sf, SHAPE_Area > 150000)
#random_sample <- sample(1:dim(props_sf)[1], 3)
#props_sf <- props_sf[random_sample,]
#props_sf <- st_centroid(props_sf)


setwd("C:/Users/slehnen/Onedrive - DOI/Ocelot_collab/work")
#saveRDS(props_sf, "example_PADS.RDS")
props_sf <- readRDS("example_PADS.RDS")
props_sf$num <- 1:3

aoi_plot <- ggplot(polys) +
  theme_few()+
  geom_sf()+
  geom_sf(data = props_sf, size = 7, shape = 23, fill = "darkred")+
  geom_sf_text(data = props_sf, aes(label = num), colour = "white")+
  xlab("")+
  ylab("")
aoi_plot

## directory to current bioclim layers
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Source/bioclim/wc2.1_2.5m_bio")
rasters.l <- list.files(pattern = "\\.tif$") 
bioclim <- stack(rasters.l)

bioclim_props <- list()
for(i in 1:dim(props_sf)[1]){
  bioclim_props[[i]] <- as.matrix(raster::extract(bioclim, props_sf[i,]))
}

bioclim_props_sum <- as.data.frame(do.call(rbind, bioclim_props))
bioclim_props_sum$patch_id <- props_sf$num 


prop_values <- bioclim_props_sum

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate.RDS")

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate.RDS")
data1_trn <- readRDS("training_data_ocelot_climate.RDS")

ocelots_data <- subset(data1_trn, type == "ocelot")
random_data <- subset(data1_trn, type == "random")


########################################
# Shapely ###############################
#########################################

bioclim_propsTransformed <- predict(preProcValues, prop_values)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata,  type = "prob")
}

# Plot individual explanations
which_patch <- 2
props_sf$num
which_patch <- props_sf$num[which_patch]

pre_patch <- predict(greedy_ensemble, newdata = bioclim_propsTransformed[props_sf$num[which_patch],], type="prob")
pre_patch

model_names <- rownames(varImp(greedy_ensemble)) 

bioclim_propsTransformed <- bioclim_propsTransformed[,names(bioclim_propsTransformed) %in% model_names]
X <- trainTransformed[,names(trainTransformed) %in% model_names]

expl <- fastshap::explain(greedy_ensemble, X = X, pred_wrapper = pfun, nsim = 1000, newdata = bioclim_propsTransformed[which_patch, ])
expl <- as.data.frame(t(expl))
expl$variables <- rownames(expl)
expl <- expl[expl$V1!=0,]

#BIO1 = Annual Mean Temperature
#
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#
#BIO3 = Isothermality (BIO2/BIO7) (?100)

#BIO4 = Temperature Seasonality (standard deviation ?100)

#BIO5 = Max Temperature of Warmest Month

#BIO6 = Min Temperature of Coldest Month

#BIO7 = Temperature Annual Range (BIO5-BIO6)

#BIO8 = Mean Temperature of Wettest Quarter

#BIO9 = Mean Temperature of Driest Quarter

#BIO10 = Mean Temperature of Warmest Quarter

#BIO11 = Mean Temperature of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precipitation of Wettest Month

#BIO14 = Precipitation of Driest Month

#BIO15 = Precipitation Seasonality (Coefficient of Variation)

#BIO16 = Precipitation of Wettest Quarter

#BIO17 = Precipitation of Driest Quarter

#BIO18 = Precipitation of Warmest Quarter

#BIO19 = Precipitation of Coldest Quarter

expl$var_names <- c(paste("Precipitation of wettest month =", round(prop_values$wc2.1_2.5m_bio_13)[which_patch], "mm"),
                    paste("Precipitation of driest month =", round(prop_values$wc2.1_2.5m_bio_14)[which_patch], "mm"), 
                    paste("Precipitation seasonality (CV) =", round(prop_values$wc2.1_2.5m_bio_15)[which_patch], "%"),
                    paste("Precipitation of warmest quarter =", round(prop_values$wc2.1_2.5m_bio_18)[which_patch], "mm"),
                    paste("Precipitation of coldest quarter =", round(prop_values$wc2.1_2.5m_bio_19)[which_patch], "mm"), 
                    paste("Mean diurnal range =", round(prop_values$wc2.1_2.5m_bio_2)[which_patch], "\u00B0C"), 
                    paste("Isothermality =", round(prop_values$wc2.1_2.5m_bio_3)[which_patch], ""), 
                    paste("Mean temperature of wettest quarter =", round(prop_values$wc2.1_2.5m_bio_8)[which_patch], "\u00B0C"),
                    paste("Mean temperature of driest quarter =", round(prop_values$wc2.1_2.5m_bio_9)[which_patch], "\u00B0C"))

# Reorder following the value of another column:
western_point2 <- expl %>%
  mutate(Bioclim.variable = fct_reorder(var_names, V1)) %>%
  ggplot( aes(x=Bioclim.variable, y=V1)) +
  geom_bar(stat="identity", alpha= 0.6, width= 0.4) +
  ggtitle(paste("(b) Site 1", "ensemble model probability =", round(pre_patch, 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  xlab("") +
  ylim(c(-0.2, 0.05))+
  ylab("SHAP value (impact on model prediction)")+
  theme_few()

western_point2


# Plot individual explanations
which_patch <- 3
props_sf$num
which_patch <- props_sf$num[which_patch]

pre_patch <- predict(greedy_ensemble, newdata = bioclim_propsTransformed[props_sf$num[which_patch],], type="prob")
pre_patch

model_names <- rownames(varImp(greedy_ensemble)) 

bioclim_propsTransformed <- bioclim_propsTransformed[,names(bioclim_propsTransformed) %in% model_names]
X <- trainTransformed[,names(trainTransformed) %in% model_names]

expl <- fastshap::explain(greedy_ensemble, X = X, pred_wrapper = pfun, nsim = 1000, newdata = bioclim_propsTransformed[which_patch, ])
expl <- as.data.frame(t(expl))
expl$variables <- rownames(expl)
expl <- expl[expl$V1!=0,]

#BIO1 = Annual Mean Temperature
#
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#
#BIO3 = Isothermality (BIO2/BIO7) (?100)

#BIO4 = Temperature Seasonality (standard deviation ?100)

#BIO5 = Max Temperature of Warmest Month

#BIO6 = Min Temperature of Coldest Month

#BIO7 = Temperature Annual Range (BIO5-BIO6)

#BIO8 = Mean Temperature of Wettest Quarter

#BIO9 = Mean Temperature of Driest Quarter

#BIO10 = Mean Temperature of Warmest Quarter

#BIO11 = Mean Temperature of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precipitation of Wettest Month

#BIO14 = Precipitation of Driest Month

#BIO15 = Precipitation Seasonality (Coefficient of Variation)

#BIO16 = Precipitation of Wettest Quarter

#BIO17 = Precipitation of Driest Quarter

#BIO18 = Precipitation of Warmest Quarter

#BIO19 = Precipitation of Coldest Quarter

expl$var_names <- c(paste("Precipitation of wettest month =", round(prop_values$wc2.1_2.5m_bio_13)[which_patch], "mm"),
                    paste("Precipitation of driest month =", round(prop_values$wc2.1_2.5m_bio_14)[which_patch], "mm"), 
                    paste("Precipitation seasonality (CV) =", round(prop_values$wc2.1_2.5m_bio_15)[which_patch], "%"),
                    paste("Precipitation of warmest quarter =", round(prop_values$wc2.1_2.5m_bio_18)[which_patch], "mm"),
                    paste("Precipitation of coldest quarter =", round(prop_values$wc2.1_2.5m_bio_19)[which_patch], "mm"), 
                    paste("Mean diurnal range =", round(prop_values$wc2.1_2.5m_bio_2)[which_patch], "\u00B0C"), 
                    paste("Isothermality =", round(prop_values$wc2.1_2.5m_bio_3)[which_patch], ""), 
                    paste("Mean temperature of wettest quarter =", round(prop_values$wc2.1_2.5m_bio_8)[which_patch], "\u00B0C"),
                    paste("Mean temperature of driest quarter =", round(prop_values$wc2.1_2.5m_bio_9)[which_patch], "\u00B0C"))

# Reorder following the value of another column:
western_point3 <- expl %>%
  mutate(Bioclim.variable = fct_reorder(var_names, V1)) %>%
  ggplot( aes(x=Bioclim.variable, y=V1)) +
  geom_bar(stat="identity", alpha= 0.6, width= 0.4) +
  ggtitle(paste("(c) Site 2", "ensemble model probability =", round(pre_patch, 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  xlab("") +
 ylim(c(-0.2, 0.05))+
  ylab("SHAP value (impact on model prediction)")+
  theme_few()

western_point3

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/source")
ex_pop2 <- st_read("Texas_ocelot_current.shp")
ex_pop <- readOGR(dsn=getwd(), layer="Texas_ocelot_current")

bioclim_ex_pop <- list()
for(i in 1:length(ex_pop)){
  print(i)
  bioclim_ex_pop[[i]] <- colMeans(as.matrix(raster::extract(bioclim, ex_pop[i,])[[1]]))
}

ex_pop_data <- as.data.frame(do.call(rbind, bioclim_ex_pop))
ex_pop_data_sum <- as.data.frame(t(colMeans(ex_pop_data)))
ex_pop_data_sum_Transformed <- predict(preProcValues, ex_pop_data_sum)
# Plot individual explanations

pre_patch <- predict(greedy_ensemble, newdata = ex_pop_data_sum_Transformed, type="prob")
pre_patch

model_names <- rownames(varImp(greedy_ensemble)) 

ex_pop_data_sum_Transformed <- ex_pop_data_sum_Transformed[,names(ex_pop_data_sum_Transformed) %in% model_names]
X <- trainTransformed[,names(trainTransformed) %in% model_names]

expl <- fastshap::explain(greedy_ensemble, X = X, pred_wrapper = pfun, nsim = 500, newdata = ex_pop_data_sum_Transformed)
expl <- as.data.frame(t(expl))
expl$variables <- rownames(expl)
expl <- expl[expl$V1!=0,]

#BIO1 = Annual Mean Temperature
#
#BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
#
#BIO3 = Isothermality (BIO2/BIO7) (?100)

#BIO4 = Temperature Seasonality (standard deviation ?100)

#BIO5 = Max Temperature of Warmest Month

#BIO6 = Min Temperature of Coldest Month

#BIO7 = Temperature Annual Range (BIO5-BIO6)

#BIO8 = Mean Temperature of Wettest Quarter

#BIO9 = Mean Temperature of Driest Quarter

#BIO10 = Mean Temperature of Warmest Quarter

#BIO11 = Mean Temperature of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precipitation of Wettest Month

#BIO14 = Precipitation of Driest Month

#BIO15 = Precipitation Seasonality (Coefficient of Variation)

#BIO16 = Precipitation of Wettest Quarter

#BIO17 = Precipitation of Driest Quarter

#BIO18 = Precipitation of Warmest Quarter

#BIO19 = Precipitation of Coldest Quarter

expl$var_names <- c(paste("Precipitation of wettest month =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_13"]), "mm"),
                    paste("Precipitation of driest month =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_14"]), "mm"), 
                    paste("Precipitation seasonality (CV) =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_15"]), "%"),
                    paste("Precipitation of warmest quarter =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_18"]), "mm"),
                    paste("Precipitation of coldest quarter =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_19"]), "mm"), 
                    paste("Mean diurnal range =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_2"]), "\u00B0C"), 
                    paste("Isothermality =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_3"]), ""), 
                    paste("Mean temperature of wettest quarter =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_8"]), "\u00B0C"),
                    paste("Mean temperature of driest quarter =", round(bioclim_ex_pop[[1]][names(bioclim_ex_pop[[1]]) == "wc2.1_2.5m_bio_9"]), "\u00B0C"))


# Reorder following the value of another column:
exist_pop <- expl %>%
  mutate(Bioclim.variable = fct_reorder(var_names, V1)) %>%
  ggplot( aes(x=Bioclim.variable, y=V1)) +
  geom_bar(stat="identity", alpha= 0.6, width= 0.4) +
  ggtitle(paste("(a) Existing habitat ensemble model probability =", round(pre_patch, 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  xlab("") +
  ylim(c(-0.2, 0.05))+
  ylab("SHAP value (impact on model prediction)")+
  theme_few()
exist_pop

aoi_exist <- ggplot(polys) +
  theme_few()+
  geom_sf(size = 1.1)+
  geom_sf(data = ex_pop2, size = 1, shape = 1, colour = "black", fill = "black")+
#  geom_sf_text(data = ex_pop, aes(label = num), colour = "white")+
  xlab("")+
  ylab("")+ theme(axis.text.x=element_blank(), #remove x axis labels
                  axis.ticks.x=element_blank(), #remove x axis ticks
                  axis.text.y=element_blank(),  #remove y axis labels
                  axis.ticks.y=element_blank()  #remove y axis ticks
  )+  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "white")
  )
aoi_exist 

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg(filename = "exist_map.jpeg", width = 500, height = 500,
     pointsize = 12, quality = 100, bg = "white", res = 200)
aoi_exist
dev.off()

## add map existing
library(jpeg)
exist_map <- readJPEG('exist_map.jpeg')
exist_pop2 <- exist_pop + annotation_raster(exist_map, ymin = -0.22,ymax= -0.13,xmin = 4.5,xmax = 8.5)

## add map 2

aoi_map2 <- ggplot(polys) +
  theme_few()+
  geom_sf(size = 1.1)+
  geom_sf(data = props_sf[2,], size = 6, shape = 23, fill = "black")+
  #  geom_sf_text(data = ex_pop, aes(label = num), colour = "white")+
  xlab("")+
  ylab("")+ theme(axis.text.x=element_blank(), #remove x axis labels
                  axis.ticks.x=element_blank(), #remove x axis ticks
                  axis.text.y=element_blank(),  #remove y axis labels
                  axis.ticks.y=element_blank()  #remove y axis ticks
  )+  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "white")
  )
aoi_map2 

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg(filename = "map2.jpeg", width = 500, height = 500,
     pointsize = 12, quality = 100, bg = "white", res = 200)
aoi_map2
dev.off()

map2 <- readJPEG('map2.jpeg')
map2_add <- western_point2 + annotation_raster(map2, ymin = -0.22,ymax= -0.13,xmin = 4.5,xmax = 8.5)


## add map 3

aoi_map3 <- ggplot(polys) +
  theme_few()+
  geom_sf(size = 1.1)+
  geom_sf(data = props_sf[3,], size = 6, shape = 23, fill = "black")+
  #  geom_sf_text(data = ex_pop, aes(label = num), colour = "white")+
  xlab("")+
  ylab("")+ theme(axis.text.x=element_blank(), #remove x axis labels
                  axis.ticks.x=element_blank(), #remove x axis ticks
                  axis.text.y=element_blank(),  #remove y axis labels
                  axis.ticks.y=element_blank()  #remove y axis ticks
  )+  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "white")
  )
aoi_map3 

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg(filename = "map3.jpeg", width = 500, height = 500,
     pointsize = 12, quality = 100, bg = "white", res = 200)
aoi_map3
dev.off()

map3 <- readJPEG('map3.jpeg')
map3_add <- western_point3 + annotation_raster(map3, ymin = -0.22,ymax= -0.13,xmin = 4.5,xmax = 8.5)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg(filename = "shapley.jpeg", width = 2500, height = 3500,
     pointsize = 12, quality = 100, bg = "white", res = 300)
ggarrange(exist_pop2, map2_add, map3_add, nrow = 3, ncol = 1)
dev.off()

