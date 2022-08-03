##################################
#### ocelot climate analysis #####
#### model building          #####
#### June 7, 2022            #####
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
# the below package is used to generate random locations according to 
# the probability of a raster. It is not available on Cran
#install.packages("remotes")
#remotes::install_github("adamlilith/enmSdm") 
library(enmSdm)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Scripts/GitHub/Data")

pred_zone <- st_read(dsn = "prediction_zone.shp") # read in shapefile defining zone for prediction

aoi <- st_read(dsn = "AOI.shp")# read in shapefile defining area used in analysis

ocelot_locs <- st_read(dsn = "ocelot_locs.shp") # read in ocelot locations

inat <- raster("inaturalist_mammal_survey_effort_1970_to_2021.tiff") # raster of density of iNaturalist mammal survey effort

# Crop and mask iNaturalist raster to Area of Interest
inat <- raster::crop(inat, aoi)
inat <- raster::mask(inat, aoi)

# Create random points (for pseudo-absence or background points), with probability dependent on iNaturalist survey effort 
set.seed(1111)
random_points <- sampleRast(inat, dim(ocelot_locs)[1]*4, adjArea = TRUE, replace = FALSE, prob = TRUE)

###################################################################
################ Mask out developed and cropland areas ############
###################################################################

# raster available here: https://2018mexicolandcover10m.esa.int/. Rasters are available separately for
# Mexico and the rest of Central America. The below map has these two rasters combined with areas classified as 
# 4 (cropland) or 8 (built-up areas) converted to one(1) and all other land classes set to zero(0). 
dev <- raster("built_cropland_mask.tif") #raster with areas classified as developed (built) or cropland as 1

dev_ran <- raster::extract(x = dev, y = random_points)
dev_ocelot <- raster::extract(x = dev, y = ocelot_locs)

# The below removes random and ocelot records taken within areas classified as cropland
# or built-up areas. This step was performed to separate absences due to climate variables
# from absences due to highly altered habitat
index <- which(dev_ran>0)
random_points <- random_points[-index,]
index <- which(dev_ocelot>0)
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

####### randomly choose remaining background points equal to number of ocelot locations (n = 289)
random_points <- SpatialPointsDataFrame(coords = random_points, data = data.frame(type = rep("random", length=dim(random_points)[1])), proj4string= crs(ocelot_locs))
set.seed(1111)
index <- sample(1:dim(random_points)[1], replace = FALSE, size = dim(ocelot_locs)[1])
random_points <- random_points[index,]

# create plot of random (background) and ocelot locations
plot(random_points, col ="yellow")
plot(ocelot_locs$geometry, col = "red", add= TRUE)
plot(pred_zone[2], main = "Ocelot records (red) and background points (yellow)", add= TRUE)
plot(random_points, col ="yellow", add= TRUE)
plot(ocelot_locs$geometry, col = "red", add= TRUE)

## get Bioclim variables at 2.5 minute resolution: https://www.worldclim.org/data/worldclim21.html 
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Source/bioclim/wc2.1_2.5m_bio")
rasters.l <- list.files(pattern = "\\.tif$") 
bioclim <- stack(rasters.l)

bioclim_ocelot <- extract(bioclim, ocelot_locs)
bioclim_random <- extract(bioclim, random_points)


bioclim_ocelot <- as.data.frame(bioclim_ocelot)
bioclim_ocelot$type <- "ocelot"

bioclim_random <- as.data.frame(bioclim_random)
bioclim_random$type <- "random"

all_data <- rbind(bioclim_ocelot, bioclim_random)
all_data <- na.omit(all_data)

summary(all_data)
dim(all_data)

######################################################
#### Remove variables with high VIFs (>10)  ##########
######################################################

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

## nothing above 10 VIF

##########################################################
### Create factor for analysis in caret ##################
##########################################################

all_data$type <- factor(all_data$type)

all_data <- na.omit(all_data)
dim(all_data)

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
saveRDS(data1_trn, "training_data_ocelot_climate.RDS")
saveRDS(preProcValues, "preProcValues_ocelot_climate.RDS")
saveRDS(trainTransformed, "trainTransformed_ocelot_climate.RDS")
saveRDS(testTransformed, "testTransformed_ocelot_climate.RDS")
preProcValues <- readRDS("preProcValues_ocelot_climate.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate.RDS")

my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(trainTransformed$type, 25),
  summaryFunction=twoClassSummary
)

modelTypes <- list(glm      = caretModelSpec(method="glm"),
                   rf    = caretModelSpec(method="rf"),
                   xgbTree = caretModelSpec(method="xgbTree"),
                   svmRadial= caretModelSpec(method="svmRadial"),
                   gbm      = caretModelSpec(method="gbm", verbose=FALSE, tuneGrid=expand.grid(n.trees = c(700, 800, 900, 1000), interaction.depth = c(2, 3, 4, 5), shrinkage =0.1, n.minobsinnode = 10 )) 
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

varImp(greedy_ensemble)

saveRDS(greedy_ensemble, "greedy_ensemble_ocelot_climate.RDS")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate.RDS")

# Predict
stack_val_preds <- data.frame(predict(greedy_ensemble, trainTransformed, type = "prob"))

prob <- data.frame(predict(greedy_ensemble, testTransformed, type = "prob"))

##################################################
##### AUC calculations ############################
####################################################


# Compute AUC for predicting Class with the model

pred <- prediction(1-prob, factor(as.numeric(testTransformed[,"type"])-1))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
summary(greedy_ensemble)

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

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Source/bioclim/wc2.1_2.5m_bio")
rasters.l <- list.files(pattern = "\\.tif$") 

all <- raster::stack(rasters.l)

index <- which(names(all) %in% rownames(varImp(greedy_ensemble)))

all <- all[[index]]

index <- which(rownames(varImp(greedy_ensemble)) %in% names(all))
rownames(varImp(greedy_ensemble))[-index]

all2 <- raster::crop(all, pred_zone)
all2 <- raster::mask(all2, pred_zone)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

model.predict <- function(newdat){
  predict(greedy_ensemble, newdat, type = "prob")
}


startTime=date()
startTime

xseq <- seq(-117.2836, -77.5351, length.out = 13)

for(i in 1:12){
  print(i)
  
  alli <- crop(all2, extent(xseq[i], xseq[i+1], -53.875, 72))
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
  writeRaster(gcw.mod2, filename=paste("ocelot_climate_current", i, ".tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
}


library(raster)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

list2 <- list()
for(i in 1:12){ 
  rx <- raster(paste("ocelot_climate_current", i, ".tif", sep=""))
  list2[[i]] <- rx
}
#list2<-list2[-which(sapply(list2, is.null))]
# mosaic them, plot mosaic & save output
list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("E:/New_climate_products")
writeRaster(rast.mosaic, filename = paste("ocelot_climate_current.tif", sep = ""),
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

######################################################################
############## Future predictions ####################################
######################################################################

#############################################
########## 2050 #############################
#############################################

setwd("E:/Climate_future_CPI6_2050")
rasters.l <- list.files(pattern = "\\.tif$") 

raster_names <- unlist(strsplit(rasters.l, split = ".tif"))

bioclim_names <- c("wc2.1_2.5m_bio_1",  "wc2.1_2.5m_bio_2",
                   "wc2.1_2.5m_bio_3",  "wc2.1_2.5m_bio_4",
                   "wc2.1_2.5m_bio_5",  "wc2.1_2.5m_bio_6", 
                   "wc2.1_2.5m_bio_7",  "wc2.1_2.5m_bio_8",
                   "wc2.1_2.5m_bio_9", "wc2.1_2.5m_bio_10",
                   "wc2.1_2.5m_bio_11", "wc2.1_2.5m_bio_12",
                   "wc2.1_2.5m_bio_13", "wc2.1_2.5m_bio_14",
                   "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_16",
                   "wc2.1_2.5m_bio_17", "wc2.1_2.5m_bio_18",
                   "wc2.1_2.5m_bio_19")

for(i in 1:length(rasters.l)){
  print(i)
  setwd("E:/Climate_future_CPI6_2050")
future_climate <- stack(rasters.l[i])

names(future_climate) <- bioclim_names

## test if temperature stored as integer
divide_by <- ifelse(cellStats(future_climate[["wc2.1_2.5m_bio_8"]], max) >100, 10, 1)

index <- which(names(future_climate) %in% rownames(varImp(greedy_ensemble)))

future_climate <- future_climate[[index]]

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")

model.predict <- function(newdat){
  predict(greedy_ensemble, newdat, type = "prob")
}


startTime=date()
startTime

xseq <- seq(-117.2836, -77.5351, length.out = 13)

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
     #ensure units same as original (some temps multiplied by 10 and stored as integers to save space)
      #check and fix as needed 
    
    samp.data$wc2.1_2.5m_bio_8 <- samp.data$wc2.1_2.5m_bio_8/divide_by
    samp.data$wc2.1_2.5m_bio_2 <- samp.data$wc2.1_2.5m_bio_2/divide_by
    samp.data$wc2.1_2.5m_bio_9 <- samp.data$wc2.1_2.5m_bio_9/divide_by
      
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
  setwd("H:/new_climate_output")
  crs(gcw.mod2) <- crs(all)
  writeRaster(gcw.mod2, filename=paste("ocelot_2050", raster_names[i], "_", s, ".tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
}


library(raster)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")
setwd("H:/new_climate_output")
list2 <- list()
for(s in 1:12){ 
  rx <- raster(paste("ocelot_2050", raster_names[i], "_", s, ".tif", sep=""))
  list2[[s]] <- rx
}
#list2<-list2[-which(sapply(list2, is.null))]
# mosaic them, plot mosaic & save output
list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_change_predictions_products")
setwd("E:/New_climate_products")
writeRaster(rast.mosaic, filename = paste("ocelot_2050",  raster_names[i], ".tif", sep = ""),
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)
}

####################################################
########## 2070 #############################
#####################################################

######################################################################
############## Future predictions ########################################
######################################################################

setwd("E:/Climate_future_CPI6_2070")
rasters.l <- list.files(pattern = "\\.tif$") 

raster_names <- unlist(strsplit(rasters.l, split = ".tif"))

bioclim_names <- c("wc2.1_2.5m_bio_1",  "wc2.1_2.5m_bio_2",
                   "wc2.1_2.5m_bio_3",  "wc2.1_2.5m_bio_4",
                   "wc2.1_2.5m_bio_5",  "wc2.1_2.5m_bio_6", 
                   "wc2.1_2.5m_bio_7",  "wc2.1_2.5m_bio_8",
                   "wc2.1_2.5m_bio_9", "wc2.1_2.5m_bio_10",
                   "wc2.1_2.5m_bio_11", "wc2.1_2.5m_bio_12",
                   "wc2.1_2.5m_bio_13", "wc2.1_2.5m_bio_14",
                   "wc2.1_2.5m_bio_15", "wc2.1_2.5m_bio_16",
                   "wc2.1_2.5m_bio_17", "wc2.1_2.5m_bio_18",
                   "wc2.1_2.5m_bio_19")

for(i in 1:length(raster_names)){
  print(i)
  setwd("E:/Climate_future_CPI6_2070")
  future_climate <- stack(rasters.l[i])
  names(future_climate) <- bioclim_names
  divide_by <- ifelse(cellStats(future_climate[["wc2.1_2.5m_bio_8"]], max) >100, 10, 1)
  

  
  
  index <- which(names(future_climate) %in% rownames(varImp(greedy_ensemble)))
  
  future_climate <- future_climate[[index]]
  
  setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")
  
  model.predict <- function(newdat){
    predict(greedy_ensemble, newdat, type = "prob")
  }
  
  
  startTime=date()
  startTime
  
  xseq <- seq(-117.2836, -77.5351, length.out = 13)
  
  for(s in 1:12){
   # print(s)
    
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
       #ensure units same as original (some temps multipled by 10 and stored as integers to save space)
      #check and fix as needed 
        samp.data$wc2.1_2.5m_bio_8 <- samp.data$wc2.1_2.5m_bio_8/divide_by
        samp.data$wc2.1_2.5m_bio_2 <- samp.data$wc2.1_2.5m_bio_2/divide_by
        samp.data$wc2.1_2.5m_bio_9 <- samp.data$wc2.1_2.5m_bio_9/divide_by

        
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
    setwd("H:/new_climate_output")
    crs(gcw.mod2) <- crs(all)
    writeRaster(gcw.mod2, filename=paste("ocelot_2070", raster_names[i], "_", s, ".tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
  }
  
  
  library(raster)
  
  setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_envelope_predictions")
  setwd("H:/new_climate_output")
  list2 <- list()
  for(s in 1:12){ 
    rx <- raster(paste("ocelot_2070", raster_names[i], "_", s, ".tif", sep=""))
    list2[[s]] <- rx
  }
  #list2<-list2[-which(sapply(list2, is.null))]
  # mosaic them, plot mosaic & save output
  list2$fun   <- max
  rast.mosaic <- do.call(mosaic,list2)
  plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)
  
  crs(rast.mosaic) <- crs(rx)
  setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work/Climate_change_predictions_products")
  setwd("E:/New_climate_products")
  writeRaster(rast.mosaic, filename = paste("ocelot_2070",  raster_names[i], ".tif", sep = ""),
              format = "GTiff", datatype="FLT4S", overwrite = TRUE)
}


#################################################
### Median and Std ##############################
#################################################


######### 2050
setwd("E:/New_climate_products")

raster_list <- list.files(pattern = "\\.tif$") 
index2 <- grep("2041-2060", raster_list)
raster_list <- raster_list[index2]

climate_2050 <- stack(raster_list)
median_climate_2050 <- calc(climate_2050, median)
plot(median_climate_2050)
stdev_climate_2050 <- calc(climate_2050, sd)
plot(stdev_climate_2050)

setwd("E:/New_climate_products")

writeRaster(median_climate_2050, filename = "ocelot_climate_median_2050.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

writeRaster(stdev_climate_2050, filename = "ocelot_climate_std_2050.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

######### 2070
setwd("E:/New_climate_products")

raster_list <- list.files(pattern = "\\.tif$") 
index2 <- grep("2061-2080", raster_list)
raster_list <- raster_list[index2]

climate_2070 <- stack(raster_list)

## different extents

median_climate_2070 <- calc(climate_2070, median)
plot(median_climate_2070)
stdev_climate_2070 <- calc(climate_2070, sd)
plot(stdev_climate_2070)

setwd("E:/New_climate_products")

writeRaster(median_climate_2070, filename = "ocelot_climate_median_2070.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)

writeRaster(stdev_climate_2070, filename = "ocelot_climate_std_2070.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)



