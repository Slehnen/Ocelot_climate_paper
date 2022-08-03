library(iml)
library(caret)
library(gbm)
library("ggplot2")
library(mefa)
library(randomForest)
library(rpart)
library(rpart.plot)


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate.RDS")

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate.RDS")
data1_trn <- readRDS("training_data_ocelot_climate.RDS")
data1_trn <- data1_trn[,-10] #
names(data1_trn) <- c("precip. wet mo (mm)",
                      "precip. dry mo (mm)", 
                      "precip. CV",  
                      "precip. warm qrt (mm)",
                      "precip. cold qrt (mm)",
                      "diur. range (\u00B0C)",
                      "Isothermality",
                      "temp. wet qrt (\u00B0C)",
                      "temp. dry qrt (\u00B0C)")

trainTransformed <- trainTransformed[,-10] # remove response variable

model_predictions <- predict(greedy_ensemble, trainTransformed, type = "prob")
data1_trn$model_pre <- model_predictions

global_explainer <- rpart(model_pre~. , data=data1_trn, method="anova") 
global_explainer

# get index of CP with lowest xerror
opt <- which.min(global_explainer$cptable[,"xerror"])
#get its value
cp <- global_explainer$cptable[opt, "CP"]
#prune tree
pruned_model <- rpart::prune(global_explainer,cp)
#plot tree
rpart.plot(global_explainer)
rpart.plot(pruned_model)

rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, cex = 1.0, digits = 2)
rpart.rules(pruned_model)


y_hat_glob <- predict(global_explainer, data1_trn)
y_hat_prune <- predict(pruned_model, data1_trn)
# r squared
(cor(data1_trn$model_pre, y_hat_glob))^2 
(cor(data1_trn$model_pre, y_hat_prune))^2 

# pruned model is the same as global explainer

plot(data1_trn$model_pre, y_hat_glob)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, cex = 1.2, digits = 2)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg("explainer_model.jpeg", quality = 100, width = 1000, height =700)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, cex = 1.2, digits = 2)
dev.off()