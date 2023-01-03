##################################
#### ocelot climate analysis #####
#### model building          #####
#### October 31, 2022            #####
##################################

# Load packages

library(caret)
library(caretEnsemble)
library(gbm)
library(dplyr)
library(foreach)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(ROCR)
library(sf)
library(spatialEco)
library(ggplot2)
library(ggthemes)
library(forcats)
library(dplyr)
library(randomForest)
library(xgboost)
library(kernlab)
library(caTools)
library(DALEX)
library(BiodiversityR)

# the below package is used to generate random locations according to 
# the probability of a raster. It is not available on Cran
#install.packages("remotes")
#remotes::install_github("adamlilith/enmSdm") 
library(enmSdm)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Scripts/GitHub_chel/Data")
#setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/source")

#ocelot_locs <- st_read(dsn = "ocelot_locs_10_26_22_lit_inlcuded.shp") # read in ocelot locations

#thin to 5 km
#ocelot_locs_thin <- ensemble.spatialThin(st_coordinates(ocelot_locs), thin.km = 5, 
#                     runs = 100, silent = FALSE, verbose = TRUE, 
#                     return.notRetained = TRUE)

#index <- rownames(ocelot_locs_thin$loc.out)

#ocelot_locs <- ocelot_locs[index,]

#st_write(ocelot_locs, dsn = "ocelot_locations_thin_10_27_22.shp")

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/source")

ocelot_locs <- st_read(dsn = "ocelot_locations_thin_10_27_22.shp") # read in ocelot locations
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Scripts/GitHub_chel/Data")
inat <- raster("inaturalist_mammal_survey_effort_1970_to_2021.tiff") # raster of density of iNaturalist mammal survey effort

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Source")
aoi <- st_read(dsn ="AOI_climate_availability_2_10_22.shp")

# Crop and mask iNaturalist raster to Area of Interest
inat <- raster::crop(inat, aoi)
inat <- raster::mask(inat, aoi)

# Create random points (for pseudo-absence or background points), with probability dependent on iNaturalist survey effort 
set.seed(1111)
random_points <-enmSdm::sampleRast(inat, dim(ocelot_locs)[1]*4, adjArea = TRUE, replace = FALSE, prob = TRUE)

###################################################################
################ Mask out developed and cropland areas ############
###################################################################

# raster available here: http://www.earthenv.org/landcover. Rasters are available separately for
# by land class type. The below masked out areas classified as class 9 (Urban/Built-up) and class 7 (Cultivated and Managed Vegetation)
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Scripts/GitHub_chel/Data")

dev <- raster("consensus_full_class_9_aoi.tif") #raster with areas classified as developed (built) or cropland as 1

dev_ran <- raster::extract(x = dev, y = random_points)
dev_ocelot <- raster::extract(x = dev, y = ocelot_locs)

# The below removes random and ocelot records taken within areas classified as cropland
# or built-up areas. This step was performed to separate absences due to climate variables
# from absences due to highly altered habitat
index <- which(dev_ran>50)
random_points <- random_points[-index,]
index <- which(dev_ocelot>50)
ocelot_locs <- ocelot_locs[-index,]

ag <- raster("consensus_full_class_7_aoi.tif") #raster with areas classified as developed (built) or managed vegetation as 1

ag_ran <- raster::extract(x = ag, y = random_points)
ag_ocelot <- raster::extract(x = ag, y = ocelot_locs)

# The below removes random and ocelot records taken within areas classified as majority (>=50%) managed vegetation
# or developed areas. This step was performed to separate absences due to climate variables
# from absences due to highly altered habitat
index <- which(ag_ran>50)
random_points <- random_points[-index,]
index <- which(ag_ocelot>50)
ocelot_locs <- ocelot_locs[-index,]

#### Remove random locations within 5 km of ocelot locations
library(raster)

d <- pointDistance(coordinates(random_points), cbind(as.numeric(as.character(ocelot_locs$longitude)), as.numeric(as.character(ocelot_locs$latitude))), lonlat=TRUE, allpairs=T) 
i <- apply(d, 1, which.min)
distance = d[cbind(1:nrow(d), i)]
head(distance) # Distance from random location to nearest ocelot location
index <- which(distance<5000)
random_points <- random_points[-index,] # remove 31 random locations

# Check for and remove duplicates in both observed and random locations
pts <- rbind(random_points, sf::st_coordinates(ocelot_locs))
index <- duplicated(pts) # check for duplicates 
ocelot <- index[1:dim(ocelot_locs)[1]] # all values are false, meaning no duplicate locations in ocelot records
ran <- index[(dim(ocelot_locs)[1]+1):length(index)]
random_points <- random_points[ran==FALSE,] # remove duplicates in random locations

# create plot of ocelot locations
plot(st_geometry(aoi), border = 'grey', 
     axes = TRUE)
plot(st_geometry(ocelot_locs), pch = 3, col = 'red', add = TRUE)

## get Bioclim variables at 2.5 minute resolution: https://www.worldclim.org/data/worldclim21.html 
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/chelsea")
rasters.l <- list.files(pattern = "\\.tif$") 
bioclim <- raster::stack(rasters.l)
names(bioclim) <-  c("CHELSA_bio1_1981.2010_V.2.1", "CHELSA_bio10_1981.2010_V.2.1",
                               "CHELSA_bio11_1981.2010_V.2.1", "CHELSA_bio12_1981.2010_V.2.1",
                               "CHELSA_bio13_1981.2010_V.2.1", "CHELSA_bio14_1981.2010_V.2.1",
                               "CHELSA_bio15_1981.2010_V.2.1", "CHELSA_bio16_1981.2010_V.2.1",
                               "CHELSA_bio17_1981.2010_V.2.1", "CHELSA_bio18_1981.2010_V.2.1",
                               "CHELSA_bio19_1981.2010_V.2.1", "CHELSA_bio2_1981.2010_V.2.1", 
                               "CHELSA_bio3_1981.2010_V.2.1",  "CHELSA_bio4_1981.2010_V.2.1", 
                               "CHELSA_bio5_1981.2010_V.2.1",   "CHELSA_bio6_1981.2010_V.2.1", 
                               "CHELSA_bio7_1981.2010_V.2.1", "CHELSA_bio8_1981.2010_V.2.1", 
                               "CHELSA_bio9_1981.2010_V.2.1")

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/source")
soil <- raster("clay_pct_15_30cm_aoi_resample_na_removed.tif")

bioclim_ocelot <- raster::extract(bioclim, ocelot_locs)
bioclim_random <- raster::extract(bioclim, random_points)

soil_ocelot <- raster::extract(soil, ocelot_locs)
soil_random <- raster::extract(soil, random_points)

bioclim_ocelot <- cbind(bioclim_ocelot, soil_ocelot)
bioclim_random <- cbind(bioclim_random, soil_random)

bioclim_ocelot <- as.data.frame(bioclim_ocelot)
bioclim_ocelot$type <- "ocelot"
names(bioclim_ocelot)[20] <- "soil"
bioclim_ocelot$longitude <- ocelot_locs$longitude
bioclim_ocelot$latitude <- ocelot_locs$latitude

bioclim_random <- as.data.frame(bioclim_random)
bioclim_random$type <- "random"
names(bioclim_random)[20] <- "soil"
bioclim_random$longitude <- random_points[,1]
bioclim_random$latitude <- random_points[,2]

all_data <- rbind(bioclim_ocelot, bioclim_random)
all_data <- na.omit(all_data)

####### randomly choose remaining background points equal to number of ocelot locations 
set.seed(1111)
index_oc <- which(all_data$type == "ocelot")
index_ran <- which(all_data$type == "random")
index_ran2 <- sample(index_ran, length(index_oc))
all_data <- all_data[c(index_oc, index_ran2),]

all_data$type <- factor(all_data$type)
summary(all_data)
dim(all_data)
summary(factor(all_data$type))
all_data <- all_data[1:(length(all_data)-2)] #drop lat/long
######################################################
#### Remove variables with high VIFs (>10)  ##########
######################################################
cor_mat <- cor(all_data[,1:20])
diag(cor_mat)<-0
as.matrix(apply(cor_mat>0.98,1,function(a) paste0(colnames(cor_mat)[a], collapse = "")))

drops <- c("CHELSA_bio16_1981.2010_V.2.1", "CHELSA_bio17_1981.2010_V.2.1", "CHELSA_bio6_1981.2010_V.2.1")
all_data <- all_data[ , -which(names(all_data) %in% drops)]

all_data$type <- factor(all_data$type)
model1 <- glm(type ~., data = all_data, family = "binomial")
index <- which(car::vif(model1)==max(car::vif(model1)))
all_data <- all_data[,-index]
model1 <- glm(type ~., data = all_data, family = "binomial")
car::vif(model1)
index <- which(car::vif(model1)==max(car::vif(model1)))
all_data <- all_data[,-index]
model1 <- glm(type ~., data = all_data, family = "binomial")
car::vif(model1)
index <- which(car::vif(model1)==max(car::vif(model1)))
all_data <- all_data[,-index]
model1 <- glm(type ~., data = all_data, family = "binomial")
car::vif(model1)
index <- which(car::vif(model1)==max(car::vif(model1)))
all_data <- all_data[,-index]
model1 <- glm(type ~., data = all_data, family = "binomial")
car::vif(model1)
index <- which(car::vif(model1)==max(car::vif(model1)))
all_data <- all_data[,-index]
model1 <- glm(type ~., data = all_data, family = "binomial")
car::vif(model1)
index <- which(car::vif(model1)==max(car::vif(model1)))
all_data <- all_data[,-index]
model1 <- glm(type ~., data = all_data, family = "binomial")
car::vif(model1)
index <- which(car::vif(model1)==max(car::vif(model1)))
all_data <- all_data[,-index]
model1 <- glm(type ~., data = all_data, family = "binomial")
car::vif(model1)


## nothing above 5 VIF

##########################################################
### Create train/test split ##############################
##########################################################

default_idx <- createDataPartition(all_data$type, p = 0.8, list = FALSE)
data1_trn <- all_data[default_idx, ]
data1_tst <- all_data[-default_idx, ]
summary(data1_trn$type)
summary(data1_tst$type)

preProcValues <- preProcess(data1_trn, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, data1_trn)
testTransformed <- predict(preProcValues, data1_tst)

boxplot(trainTransformed[,-(which(names(trainTransformed) %in% c("type")))], col = "orange", main = "Features Boxplot")

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
saveRDS(data1_trn, "training_data_ocelot_climate_10_31_22.RDS")
saveRDS(preProcValues, "preProcValues_ocelot_climate_10_31_22.RDS")
saveRDS(trainTransformed, "trainTransformed_ocelot_climate_10_31_22.RDS")
saveRDS(testTransformed, "testTransformed_ocelot_climate_10_31_22.RDS")
preProcValues <- readRDS("preProcValues_ocelot_climate_10_31_22.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate_10_31_22.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate_10_31_22.RDS")

my_control <- trainControl(
  method="repeatedcv", 
  number=5, 
  repeats=3,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(trainTransformed$type, 25),
  summaryFunction=twoClassSummary
)
tunegrid_rf <- expand.grid(.mtry=c(1:5))

grid_xgbTree <- expand.grid(
  nrounds = 500,
  max_depth = c(2, 3, 4, 5),
  eta = c(0.01, 0.025, 0.05, 0.1, 0.3),
  gamma = c(0, 0.1, 0.3, 0.5, 0.7, 1.0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
modelTypes <- list(glm       = caretModelSpec(method="glm"),                                                                              # no tuning parameters possible for glms                                                                                           
                   rf        = caretModelSpec(method="rf",  classwt = c(0.5, 0.5), ntree = 1000, maxnodes = 2^4, tuneGrid = tunegrid_rf), #setting maxnodes to 2^5 restricts the maximum depth to 5, added to prevent overfitting
                   xgbTree   = caretModelSpec(method="xgbTree", tuneGrid = grid_xgbTree),
                   svmRadial = caretModelSpec(method="svmRadial"),                                                                          # for svmRadial, procedure automatically chooses the optimal values for the model tuning parameters                
                   gbm       = caretModelSpec(method="gbm", verbose=FALSE, tuneGrid=expand.grid(n.trees = c(700, 800, 900, 1000), interaction.depth = c(2, 3, 4, 5), shrinkage =0.1, n.minobsinnode = 10 )) 
)

model_list <- caretList(
  type~., data=trainTransformed,
  trControl=my_control,
  metric = "ROC",
  tuneList = modelTypes
)

xyplot(resamples(model_list))

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))


summary(greedy_ensemble)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
saveRDS(greedy_ensemble, "greedy_ensemble_ocelot_climate_10_31_22.RDS")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate_10_31_22.RDS")


########################################
# Variable importance #################
#######################################

y <- testTransformed$type
y <- ifelse(y=="random", 0, 1)

explainer_em <- DALEX::explain(model = greedy_ensemble, 
                               data = testTransformed[,-11], 
                               y = y)

logit <- function(x) exp(x)/(1+exp(x))
custom_loss <- function(observed, predicted){
  sum((observed - logit(predicted))^2)
}
attr(custom_loss, "loss_name") <- "Logit residuals"
set.seed(1111)
model_parts_em <- DALEX::model_parts(explainer_em, type = "raw",
                                       loss_function = custom_loss)
head(model_parts_em, 10)
plot(model_parts_em)


line_num <- model_parts_em$dropout_loss[model_parts_em$variable=="_full_model_"][1]
index <- which(model_parts_em$variable %in% c("_full_model_", "_baseline_"))
model_parts_em <- model_parts_em[-index,]

model_parts_em$var_nam <- NA
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio2_1981.2010_V.2.1"] <- "Mean diurnal range (BIO2)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio18_1981.2010_V.2.1"] <- "Precipitation of warmest quarter (BIO18)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio3_1981.2010_V.2.1"] <- "Isothermality (BIO3)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio15_1981.2010_V.2.1"] <- "Precipitation seasonality (CV) (BIO15)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio14_1981.2010_V.2.1"] <- "Precipitation of driest month (BIO14)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio9_1981.2010_V.2.1"] <- "Mean temperature of driest quarter (BIO9)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio13_1981.2010_V.2.1"] <- "Precipitation of wettest month (BIO13)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio19_1981.2010_V.2.1"] <- "Precipitation of coldest quarter (BIO19)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio8_1981.2010_V.2.1"] <- "Mean temperature of wettest quarter (BIO8)"
model_parts_em$var_nam[model_parts_em$variable == "CHELSA_bio4_1981.2010_V.2.1"] <- "Temperature Seasonality (SD ? 100) (BIO4)"
model_parts_em$var_nam[model_parts_em$variable == "soil"] <- "% clay (15-30 cm)"

explainer_em %>% model_parts() %>% plot(show_boxplots = FALSE) + ggtitle("Feature Importance ", "")

var_order <- names(sort(tapply(model_parts_em$dropout_loss, list(model_parts_em$var_nam), mean)))
model_parts_em$var_nam <- factor(model_parts_em$var_nam, levels = var_order)

var_imp <- ggplot(model_parts_em, aes(x=var_nam, dropout_loss))+
  geom_boxplot() +
  ylab("Logit residuals loss after permutations") +
  xlab("") +
  theme_few(18)+
  coord_flip()+
  geom_hline(yintercept = line_num, linetype="dashed")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
var_imp

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "variable_importance.jpeg", width = 3500, height = 3000,
     pointsize = 12, quality = 100, bg = "white", res = 300)
var_imp
dev.off()

##################################################
##### AUC calculations ############################
####################################################

# Predict

prob <- data.frame(predict(greedy_ensemble, type = "prob"))

# Compute AUC for predicting Class with the model

roc_val <-pROC::roc(predictor = prob$predict.greedy_ensemble..type....prob.., response = factor(as.numeric(trainTransformed[,"type"])-1)) 

plot(roc_val,col='blue')

#AUC on training data: area under the curve: 0.9363
pROC::auc(roc_val)

stack_val_preds <- data.frame(predict(greedy_ensemble, testTransformed, type = "prob"))
prob <- data.frame(predict(greedy_ensemble, testTransformed, type = "prob"))
roc_val <-pROC::roc(predictor = prob$predict.greedy_ensemble..testTransformed..type....prob.., response = factor(as.numeric(testTransformed[,"type"])-1)) 

plot(roc_val,col='blue')

#AUC on testing data: Area under the curve: 0.8858
pROC::auc(roc_val)

CF <- coef(greedy_ensemble$ens_model$finalModel)[-1]

abs(CF)/max(abs(CF))*100

prob[prob>0.5] <- 1
prob[prob<0.5] <- 0
tpr_data <- as.data.frame(cbind(prob, testTransformed$type))
names(tpr_data) <- c("model_predict", "actual")
ocelot_only <- subset(tpr_data, actual=="ocelot")
random_only <- subset(tpr_data, actual=="random")
TPR.Table2 <- length(ocelot_only$model_predict[ocelot_only$model_predict==1])/summary(testTransformed$type)[1] #correctly predicted presence observations divided by total number of presence observations
FPR.Table2 <- length(random_only$model_predict[random_only$model_predict==1])/dim(random_only)[1] # wrongly predicted absence observations divided by total number of absence observations
BiodiversityR::ensemble.SEDI(TPR=TPR.Table2, FPR=FPR.Table2)

plot(testTransformed$type, (predict(greedy_ensemble, testTransformed, type="prob")))


##########################################
####### Current spatial prediction #######
##########################################
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

index <- which(names(all) %in% rownames(varImp(greedy_ensemble)))

all <- all[[index]]

index <- which(rownames(varImp(greedy_ensemble)) %in% names(all))
rownames(varImp(greedy_ensemble))

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

model.predict <- function(newdat){
  predict(greedy_ensemble, newdat, type = "prob")
}


startTime=date()
startTime

xseq <- seq(-117.1251, -34.79181, length.out = 13)

for(i in 1:12){
  print(i)
  
  alli <- crop(all, extent(xseq[i], xseq[i+1], -34.97514, 32.71653))
  print("crop done")
  p <- data.frame(rasterToPoints(alli))
  print("raster to points done")
  #p <- na.omit(p)
  names(p)
  if(dim(p)[1]==0)next

  
  chunk1 <- seq(1, length(p[,1]), by=1000)
  startTime=date()
  result.1 <-
    foreach(a=chunk1)%dopar% {
      samp.data <- p[a:(a+999),]
      samp.data <- as.data.frame(samp.data)
      index <- rowSums(is.na(samp.data)) != 0
      # replace NAs in data frame with zeros
      samp.data[is.na(samp.data)] <- 0

      samp.data <- predict(preProcValues, samp.data)
      ## NAs cause problems in predictions
      #index of NA rows
      # predict
      data_out <- predict(greedy_ensemble, samp.data, type = "prob")
      # replace predictions with NA, if row contained NA
      data_out[index] <- NA
      cat('\r',a/dim(p)[1]*100); flush.console() 
      return(data_out)
    } 
  stopTime=date()
  
  gcw.mod <- unlist(result.1, use.names = FALSE) # Make list into data frame
  gcw.mod <- gcw.mod[1:length(p[,1])]
  gcw.mod1 <- cbind(p[1:length(p[,1]),1:2], gcw.mod) # Combine results with xy coordinates
  
  colnames(gcw.mod1) <- c("x", "y", "dens")
  
  gcw.mod2 <- rasterFromXYZ(gcw.mod1[,c("x", "y", "dens")]) # Make raster of predicted density
  
  setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")
  
  crs(gcw.mod2) <- crs(all)
  writeRaster(gcw.mod2, filename=paste("ocelot_climate_current_chelsea", i, ".tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
}


library(raster)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

list2 <- list()
for(i in 1:12){ 
  rx <- raster(paste("ocelot_climate_current_chelsea", i, ".tif", sep=""))
  list2[[i]] <- rx
}
#list2<-list2[-which(sapply(list2, is.null))]
# mosaic them, plot mosaic & save output
list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

writeRaster(rast.mosaic, filename = "ocelot_climate_current_chelsea_11_22_22.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

######################################################################
############## Future predictions ####################################
######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/source")
soil <- raster("clay_pct_15_30cm_aoi_resample_na_removed_for_future_modeling.tif")

setwd("C:/Users/slehnen/Downloads/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies")
rasters.l <- list.files(pattern = "\\.tif$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE) 


years <- c("2041-2070", "2071-2100")
ssp <- c("ssp126", "ssp370", "ssp585")
gcm <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
combos <- as.data.frame(expand.grid(years, ssp, gcm))

bioclim_names <- c("CHELSA_bio13_1981.2010_V.2.1",  "CHELSA_bio14_1981.2010_V.2.1",
                   "CHELSA_bio15_1981.2010_V.2.1",  "CHELSA_bio18_1981.2010_V.2.1",
                   "CHELSA_bio19_1981.2010_V.2.1",  "CHELSA_bio2_1981.2010_V.2.1", 
                   "CHELSA_bio3_1981.2010_V.2.1",  "CHELSA_bio8_1981.2010_V.2.1",
                   "CHELSA_bio9_1981.2010_V.2.1", "soil")

for(i in 1:length(combos[,1])){
  print(i)
  index <- grep(paste(combos[i,1], combos[i,3],combos[i,2], sep ="/"), rasters.l)
  if(length(index)!= 9) next
  setwd("C:/Users/slehnen/Downloads/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies")
future_climate <- stack(rasters.l[index], soil)


names(future_climate) <- bioclim_names

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

model.predict <- function(newdat){
  predict(greedy_ensemble, newdat, type = "prob")
}


startTime=date()
startTime

xseq <- seq(-117.1251, -34.79181, length.out = 13)

for(s in 1:12){

  alli <- crop(future_climate, extent(xseq[s], xseq[s+1], -38.875, 34.16667))
  print("crop done")
  p <- data.frame(rasterToPoints(alli))
  print("raster to points done")
  #p <- na.omit(p)
  names(p)
  if(dim(p)[1]==0)next
  
  
  chunk1 <- seq(1, length(p[,1]), by=1000)
  startTime=date()
  result.1 <-
    foreach(a=chunk1)%dopar% {
      samp.data <- p[a:(a+999),]
      samp.data <- as.data.frame(samp.data)
      index <- rowSums(is.na(samp.data)) != 0
      # replace NAs in data frame with zeros
      samp.data[is.na(samp.data)] <- 0
    
    samp.data$wc2.1_2.5m_bio_8 <- samp.data$wc2.1_2.5m_bio_8
    samp.data$wc2.1_2.5m_bio_2 <- samp.data$wc2.1_2.5m_bio_2
    samp.data$wc2.1_2.5m_bio_9 <- samp.data$wc2.1_2.5m_bio_9
      
      samp.data <- predict(preProcValues, samp.data)
      ## NAs cause problems in predictions
      #index of NA rows
      # predict
      data_out <- predict(greedy_ensemble, samp.data, type = "prob")
      # replace predictions with NA, if row contained NA
      data_out[index] <- NA
      cat('\r',a/dim(p)[1]*100); flush.console() 
      return(data_out)
    } 
  stopTime=date()
  
  gcw.mod <- unlist(result.1, use.names = FALSE) # Make list into data frame
  gcw.mod <- gcw.mod[1:length(p[,1])]
  gcw.mod1 <- cbind(p[1:length(p[,1]),1:2], gcw.mod) # Combine results with xy coordinates
  
  colnames(gcw.mod1) <- c("x", "y", "dens")
  
  gcw.mod2 <- rasterFromXYZ(gcw.mod1[,c("x", "y", "dens")]) # Make raster of predicted density
  
  setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")
  crs(gcw.mod2) <- crs(all)
  writeRaster(gcw.mod2, filename=paste("ocelot_", paste(combos[i,1], combos[i,3],combos[i,2], sep =""), "_", s, ".tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
}


library(raster)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

list2 <- list()
for(s in 1:12){ 
  rx <- raster(paste("ocelot_", paste(combos[i,1], combos[i,3],combos[i,2], sep =""), "_", s, ".tif", sep=""))
  list2[[s]] <- rx
}

list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_change_predictions_products")

writeRaster(rast.mosaic, filename = paste("ocelot_", paste(combos[i,1], combos[i,3],combos[i,2], sep =""), "_", ".tif", sep=""),
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)
}

#################################################
### Median and Std ##############################
#################################################


######### 2050
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_change_predictions_products")

raster_list <- list.files(pattern = "\\.tif$") 
index2 <- grep("2041-2070", raster_list)
raster_list <- raster_list[index2]

climate_2050 <- stack(raster_list)
median_climate_2050 <- calc(climate_2050, median)
plot(median_climate_2050)
stdev_climate_2050 <- calc(climate_2050, sd)
plot(stdev_climate_2050)

writeRaster(median_climate_2050, filename = "ocelot_climate_median_2050_12_1_22.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

writeRaster(stdev_climate_2050, filename = "ocelot_climate_std_2050_12_1_22.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

######### 2070
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_change_predictions_products")

raster_list <- list.files(pattern = "\\.tif$") 
index2 <- grep("2071-2100", raster_list)
raster_list <- raster_list[index2]

climate_2070 <- stack(raster_list)

## different extents

median_climate_2070 <- calc(climate_2070, median)
plot(median_climate_2070)
stdev_climate_2070 <- calc(climate_2070, sd)
plot(stdev_climate_2070)

writeRaster(median_climate_2070, filename = "ocelot_climate_median_2070_12_1_22.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

writeRaster(stdev_climate_2070, filename = "ocelot_climate_std_2070_12_1_22.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

