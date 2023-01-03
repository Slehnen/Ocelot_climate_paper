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

set.seed(1111)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Source")
aoi <- st_read(dsn ="AOI_climate_availability_2_10_22.shp")

setwd("C:/Users/slehnen/Onedrive - DOI/Ocelot_collab/source")
#saveRDS(props_sf, "example_PADS.RDS")
props_sf <- st_read(dsn ="example_sites_full_aoi.shp")
props_sf <- props_sf[,"NAME"]
props_sf$num <- c(2, 1, 4, 3)
props_sf <- props_sf[order(props_sf$num),]

aoi_plot <- ggplot(aoi) +
  theme_few(17)+
  geom_sf()+
  #geom_sf(data = props_sf, size = 1, shape = 23, fill = "darkred")+
  geom_sf_text(data = props_sf, size = 7, aes(label = num), colour = "black")+
  xlab("")+
  ylab("")
aoi_plot

## directory to current bioclim layers
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/source")
soil <- raster("clay_pct_15_30cm_aoi_resample_na_removed.tif")

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/chelsea")
rasters.l <- list.files(pattern = "\\.tif$") 
r1 <- raster(rasters.l[1])
soil <- resample(soil, r1)

bioclim <- c("CHELSA_bio1_1981.2010_V.2.1", "CHELSA_bio10_1981.2010_V.2.1",
             "CHELSA_bio11_1981.2010_V.2.1", "CHELSA_bio12_1981.2010_V.2.1",
             "CHELSA_bio13_1981.2010_V.2.1", "CHELSA_bio14_1981.2010_V.2.1",
             "CHELSA_bio15_1981.2010_V.2.1", "CHELSA_bio16_1981.2010_V.2.1",
             "CHELSA_bio17_1981.2010_V.2.1", "CHELSA_bio18_1981.2010_V.2.1",
             "CHELSA_bio19_1981.2010_V.2.1", "CHELSA_bio2_1981.2010_V.2.1", 
             "CHELSA_bio3_1981.2010_V.2.1",  "CHELSA_bio4_1981.2010_V.2.1", 
             "CHELSA_bio5_1981.2010_V.2.1",   "CHELSA_bio6_1981.2010_V.2.1", 
             "CHELSA_bio7_1981.2010_V.2.1", "CHELSA_bio8_1981.2010_V.2.1", 
             "CHELSA_bio9_1981.2010_V.2.1")
all <- stack(rasters.l, soil)
names(all)[1:19] <- bioclim
names(all)[20] <- "soil"

prop_values <- raster::extract(x=all, y=props_sf, fun=median, df=TRUE)


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate_10_31_22.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate_10_31_22.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate_10_31_22.RDS")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate_10_31_22.RDS")
data1_trn <- readRDS("training_data_ocelot_climate_10_31_22.RDS")

ocelots_data <- subset(data1_trn, type == "ocelot")
random_data <- subset(data1_trn, type == "random")


########################################
# Shapely ###############################
#########################################

index <- which(names(prop_values) %in% rownames(varImp(greedy_ensemble)))
prop_values <- prop_values[,index]

bioclim_propsTransformed <- predict(preProcValues, prop_values)

# Prediction wrapper
pfun <- function(object, newdata) {
  predict(object, newdata = newdata,  type = "prob")
}

# Plot individual explanations
which_patch <- 1
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

#BIO4 = temp. Seasonality (standard deviation ?100)

#BIO5 = Max temp. of Warmest Month

#BIO6 = Min temp. of Coldest Month

#BIO7 = temp. Annual Range (BIO5-BIO6)

#BIO8 = Mean temp. of Wettest Quarter

#BIO9 = Mean temp. of Driest Quarter

#BIO10 = Mean temp. of Warmest Quarter

#BIO11 = Mean temp. of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precip. of Wettest Month

#BIO14 = Precip. of Driest Month

#BIO15 = Precip. Seasonality (Coefficient of Variation)

#BIO16 = Precip. of Wettest Quarter

#BIO17 = Precip. of Driest Quarter

#BIO18 = Precip. of Warmest Quarter

#BIO19 = Precip. of Coldest Quarter

expl$var_names <- c(paste("Clay =", round(prop_values$soil*0.1)[which_patch], "% (15-30 cm)"),
                    paste("Precip. of wettest month =", round(prop_values$CHELSA_bio13_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of driest month =", round(prop_values$CHELSA_bio14_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Precip. seasonality (CV) =", round(prop_values$CHELSA_bio15_1981.2010_V.2.1*0.1)[which_patch], "%"),
                    paste("Precip. of warmest quarter =", round(prop_values$CHELSA_bio18_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of coldest quarter =", round(prop_values$CHELSA_bio19_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Mean diurnal range =", round(prop_values$CHELSA_bio2_1981.2010_V.2.1*0.1)[which_patch], "\u00B0C"), 
                    paste("Isothermality =", round(prop_values$CHELSA_bio3_1981.2010_V.2.1*0.1)[which_patch], ""), 
                    paste("Mean temp. of wettest quarter =", round(prop_values$CHELSA_bio8_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"),
                    paste("Mean temp. of driest quarter =", round(prop_values$CHELSA_bio9_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"))


mexico <- expl %>%
  mutate(Bioclim.variable = fct_reorder(var_names, V1)) %>%
  ggplot( aes(x=Bioclim.variable, y=V1)) +
  geom_bar(stat="identity", alpha= 0.6, width= 0.4) +
  ggtitle(paste("(a) El Cielo", "ensemble model probability =", round(pre_patch, 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  geom_hline(yintercept = 0, 
             color = "black", linewidth=0.7) +
  xlab("") +
  ylim(c(-0.2, 0.1))+
  ylab("Shapley value (impact on model prediction)")+
  theme_few(17)

mexico


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

#BIO4 = temp. Seasonality (standard deviation ?100)

#BIO5 = Max temp. of Warmest Month

#BIO6 = Min temp. of Coldest Month

#BIO7 = temp. Annual Range (BIO5-BIO6)

#BIO8 = Mean temp. of Wettest Quarter

#BIO9 = Mean temp. of Driest Quarter

#BIO10 = Mean temp. of Warmest Quarter

#BIO11 = Mean temp. of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precip. of Wettest Month

#BIO14 = Precip. of Driest Month

#BIO15 = Precip. Seasonality (Coefficient of Variation)

#BIO16 = Precip. of Wettest Quarter

#BIO17 = Precip. of Driest Quarter

#BIO18 = Precip. of Warmest Quarter

#BIO19 = Precip. of Coldest Quarter

expl$var_names <- c(paste("Clay =", round(prop_values$soil*0.1)[which_patch], "% (15-30 cm)"),
                    paste("Precip. of wettest month =", round(prop_values$CHELSA_bio13_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of driest month =", round(prop_values$CHELSA_bio14_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Precip. seasonality (CV) =", round(prop_values$CHELSA_bio15_1981.2010_V.2.1*0.1)[which_patch], "%"),
                    paste("Precip. of warmest quarter =", round(prop_values$CHELSA_bio18_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of coldest quarter =", round(prop_values$CHELSA_bio19_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Mean diurnal range =", round(prop_values$CHELSA_bio2_1981.2010_V.2.1*0.1)[which_patch], "\u00B0C"), 
                    paste("Isothermality =", round(prop_values$CHELSA_bio3_1981.2010_V.2.1*0.1)[which_patch], ""), 
                    paste("Mean temp. of wettest quarter =", round(prop_values$CHELSA_bio8_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"),
                    paste("Mean temp. of driest quarter =", round(prop_values$CHELSA_bio9_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"))

# Reorder following the value of another column:
costa <- expl %>%
  mutate(Bioclim.variable = fct_reorder(var_names, V1)) %>%
  ggplot( aes(x=Bioclim.variable, y=V1)) +
  geom_bar(stat="identity", alpha= 0.6, width= 0.4) +
  ggtitle(paste("(b) Pacuare River", "ensemble model probability =", round(pre_patch, 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  xlab("") +
  ylim(c(-0.2, 0.1))+
  ylab("Shapley value (impact on model prediction)")+
  theme_few(17)

costa

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

#BIO4 = temp. Seasonality (standard deviation ?100)

#BIO5 = Max temp. of Warmest Month

#BIO6 = Min temp. of Coldest Month

#BIO7 = temp. Annual Range (BIO5-BIO6)

#BIO8 = Mean temp. of Wettest Quarter

#BIO9 = Mean temp. of Driest Quarter

#BIO10 = Mean temp. of Warmest Quarter

#BIO11 = Mean temp. of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precip. of Wettest Month

#BIO14 = Precip. of Driest Month

#BIO15 = Precip. Seasonality (Coefficient of Variation)

#BIO16 = Precip. of Wettest Quarter

#BIO17 = Precip. of Driest Quarter

#BIO18 = Precip. of Warmest Quarter

#BIO19 = Precip. of Coldest Quarter

expl$var_names <- c(paste("Clay =", round(prop_values$soil*0.1)[which_patch], "% (15-30 cm)"),
                    paste("Precip. of wettest month =", round(prop_values$CHELSA_bio13_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of driest month =", round(prop_values$CHELSA_bio14_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Precip. seasonality (CV) =", round(prop_values$CHELSA_bio15_1981.2010_V.2.1*0.1)[which_patch], "%"),
                    paste("Precip. of warmest quarter =", round(prop_values$CHELSA_bio18_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of coldest quarter =", round(prop_values$CHELSA_bio19_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Mean diurnal range =", round(prop_values$CHELSA_bio2_1981.2010_V.2.1*0.1)[which_patch], "\u00B0C"), 
                    paste("Isothermality =", round(prop_values$CHELSA_bio3_1981.2010_V.2.1*0.1)[which_patch], ""), 
                    paste("Mean temp. of wettest quarter =", round(prop_values$CHELSA_bio8_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"),
                    paste("Mean temp. of driest quarter =", round(prop_values$CHELSA_bio9_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"))

# Reorder following the value of another column:
amazon <- expl %>%
  mutate(Bioclim.variable = fct_reorder(var_names, V1)) %>%
  ggplot( aes(x=Bioclim.variable, y=V1)) +
  geom_bar(stat="identity", alpha= 0.6, width= 0.4) +
  ggtitle(paste("(c) Tapauá", "ensemble model probability =", round(pre_patch, 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  xlab("") +
  ylim(c(-0.2, 0.1))+
  ylab("Shapley value (impact on model prediction)")+
  theme_few(17)

amazon

# Plot individual explanations
which_patch <- 4
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

#BIO4 = temp. Seasonality (standard deviation ?100)

#BIO5 = Max temp. of Warmest Month

#BIO6 = Min temp. of Coldest Month

#BIO7 = temp. Annual Range (BIO5-BIO6)

#BIO8 = Mean temp. of Wettest Quarter

#BIO9 = Mean temp. of Driest Quarter

#BIO10 = Mean temp. of Warmest Quarter

#BIO11 = Mean temp. of Coldest Quarter

#BIO12 = Annual Precipitation

#BIO13 = Precip. of Wettest Month

#BIO14 = Precip. of Driest Month

#BIO15 = Precip. Seasonality (Coefficient of Variation)

#BIO16 = Precip. of Wettest Quarter

#BIO17 = Precip. of Driest Quarter

#BIO18 = Precip. of Warmest Quarter

#BIO19 = Precip. of Coldest Quarter

expl$var_names <- c(paste("Clay =", round(prop_values$soil*0.1)[which_patch], "% (15-30 cm)"),
                    paste("Precip. of wettest month =", round(prop_values$CHELSA_bio13_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of driest month =", round(prop_values$CHELSA_bio14_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Precip. seasonality (CV) =", round(prop_values$CHELSA_bio15_1981.2010_V.2.1*0.1)[which_patch], "%"),
                    paste("Precip. of warmest quarter =", round(prop_values$CHELSA_bio18_1981.2010_V.2.1*0.1)[which_patch], "mm"),
                    paste("Precip. of coldest quarter =", round(prop_values$CHELSA_bio19_1981.2010_V.2.1*0.1)[which_patch], "mm"), 
                    paste("Mean diurnal range =", round(prop_values$CHELSA_bio2_1981.2010_V.2.1*0.1)[which_patch], "\u00B0C"), 
                    paste("Isothermality =", round(prop_values$CHELSA_bio3_1981.2010_V.2.1*0.1)[which_patch], ""), 
                    paste("Mean temp. of wettest quarter =", round(prop_values$CHELSA_bio8_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"),
                    paste("Mean temp. of driest quarter =", round(prop_values$CHELSA_bio9_1981.2010_V.2.1*0.1 - 273.15)[which_patch], "\u00B0C"))

# Reorder following the value of another column:
atlantic <- expl %>%
  mutate(Bioclim.variable = fct_reorder(var_names, V1)) %>%
  ggplot( aes(x=Bioclim.variable, y=V1)) +
  geom_bar(stat="identity", alpha= 0.6, width= 0.4) +
  ggtitle(paste("(d) Serra Geral", "ensemble model probability =", round(pre_patch, 3)))+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  geom_hline(yintercept = 0, 
             color = "black", size=0.7) +
  xlab("") +
  ylim(c(-0.2, 0.1))+
  ylab("Shapley value (impact on model prediction)")+
  theme_few(17)

atlantic

mexico_map <- ggplot(aoi) +
  theme_few(17)+
  geom_sf()+
  geom_sf_text(data = props_sf[1,], size = 7, aes(label = "x"), colour = "red")+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        panel.border = element_blank()
  )
mexico_map

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "mexico.jpeg", width = 800, height = 800,
     pointsize = 12, quality = 100, bg = "white", res = 300)
mexico_map
dev.off()


## add map existing
library(jpeg)
mexico_map <- readJPEG('mexico.jpeg')
mexico2 <- mexico + annotation_raster(mexico_map, ymin = -0.2,ymax= -0.075, xmin = 5,xmax = 11.1)
mexico2
## costa rica

costa_map <- ggplot(aoi) +
  theme_few(17)+
  geom_sf()+
  geom_sf_text(data = props_sf[2,], size = 7, aes(label = "x"), colour = "red")+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        panel.border = element_blank()
  )
costa_map

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "costa.jpeg", width = 800, height = 800,
     pointsize = 12, quality = 100, bg = "white", res = 300)
costa_map
dev.off()


## add map existing
library(jpeg)
costa_map <- readJPEG('costa.jpeg')
costa2 <- costa + annotation_raster(costa_map, ymin = -0.2,ymax= -0.075, xmin = 5,xmax = 11.1)
costa2

amazon_map <- ggplot(aoi) +
  theme_few(17)+
  geom_sf()+
  geom_sf_text(data = props_sf[3,], size = 7, aes(label = "x"), colour = "red")+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        panel.border = element_blank()
  )
amazon_map

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "amazon.jpeg", width = 800, height = 800,
     pointsize = 12, quality = 100, bg = "white", res = 300)
amazon_map
dev.off()


## add map existing
library(jpeg)
amazon_map <- readJPEG('amazon.jpeg')
amazon2 <- amazon + annotation_raster(amazon_map, ymin = -0.2,ymax= -0.075, xmin = 5,xmax = 11.1)
amazon2

atlantic_map <- ggplot(aoi) +
  theme_few(17)+
  geom_sf()+
  geom_sf_text(data = props_sf[4,], size = 7, aes(label = "x"), colour = "red")+
  xlab("")+
  ylab("")+
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        panel.border = element_blank()
  )
atlantic_map

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "atlantic.jpeg", width = 800, height = 800,
     pointsize = 12, quality = 100, bg = "white", res = 300)
atlantic_map
dev.off()


## add map existing
library(jpeg)
atlantic_map <- readJPEG('atlantic.jpeg')
atlantic2 <- atlantic + annotation_raster(atlantic_map, ymin = -0.2,ymax= -0.075, xmin = 5,xmax = 11.1)
atlantic2


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "Figure_9_shapley.jpeg", width = 3200, height = 5500,
     pointsize = 12, quality = 100, bg = "white", res = 300)
ggarrange(mexico2, costa2, amazon2, atlantic2, nrow = 4, ncol = 1)
dev.off()

