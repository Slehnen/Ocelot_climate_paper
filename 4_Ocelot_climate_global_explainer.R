library(iml)
library(caret)
library(gbm)
library("ggplot2")
library(mefa)
library(randomForest)
library(rpart)
library(rpart.plot)


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate_10_31_22.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate_10_31_22.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate_10_31_22.RDS")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate_10_31_22.RDS")
data1_trn <- readRDS("training_data_ocelot_climate_10_31_22.RDS")

data1_trn <- data1_trn[,-11] #
names(data1_trn) <- c("precip. wet mo (mm)",
                      "precip. dry mo (mm)", 
                      "precip. seasonality",  
                      "precip. warm qrt (mm)",
                      "precip. cold qrt (mm)",
                      "diur. range (\u00B0C)",
                      "isothermality",
                      "temp. wet qrt (\u00B0C)",
                      "temp. dry qrt (\u00B0C)",
                      "%clay (15-30cm)")


#### Adjust values to correct input units
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

adjust <- rep.row(rep(0.1, 10), dim((data1_trn))[1])

data_adjusted <- as.matrix(data1_trn)*adjust
data_adjusted[,8] <- data_adjusted[,8] -273.15
data_adjusted[,9] <- data_adjusted[,9] -273.15

head(data_adjusted)
data_adjusted <- as.data.frame(data_adjusted)

trainTransformed <- trainTransformed[,-11] # remove response variable

model_predictions <- predict(greedy_ensemble, trainTransformed, type = "prob")
data_adjusted$model_pre <- model_predictions

global_explainer <- rpart(model_pre~. , data=data_adjusted, method="anova") 
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


y_hat_glob <- predict(global_explainer, data_adjusted)
y_hat_prune <- predict(pruned_model, data_adjusted)
# r squared
(cor(data_adjusted$model_pre, y_hat_glob))^2 
(cor(data_adjusted$model_pre, y_hat_prune))^2 

# pruned model is the same as global explainer

plot(data_adjusted$model_pre, y_hat_glob)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, cex = 1.2, digits = 2)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg("Figure_8_explainer_model.jpeg", quality = 100, width = 3000, height = 2000, bg = "white", res = 300)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, cex = 1.2, digits = 2)
dev.off()