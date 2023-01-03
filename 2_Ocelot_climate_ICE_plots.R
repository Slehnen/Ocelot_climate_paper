require(ICEbox)
require(randomForest)
require(MASS) 

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate_10_31_22.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate_10_31_22.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate_10_31_22.RDS")
data1_trn <- readRDS("training_data_ocelot_climate_10_31_22.RDS")

y <- trainTransformed$type
X <- trainTransformed
X$type <- NULL


## import model
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate_10_31_22.RDS")

## Create an 'ice' object for the predictor "CHELSA_bio18_1981.2010_V.2.1":
pima.ice_soil = ice(object = greedy_ensemble, X = X, predictor = "soil", logodds = TRUE,
                  predictfcn = function(object, newdata){ 
                    predict(object, newdata, type = "prob")
                  }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_soil <- min(as.numeric(as.character(names(pima.ice_soil$ice_curves[1,]))))
max_soil <- max(as.numeric(as.character(names(pima.ice_soil$ice_curves[1,]))))
p_soil <- plot(pima.ice_soil, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
             centered = TRUE, ylab= "partial log-odds (centered)", xaxt  = "n",
             xlab= "% clay (15-30 cm)", bty="l", las = 1, ylim = c(-1, 1.5)) 
axis(1, at=seq(min_soil, max_soil, length.out = 5), labels=round(seq(min(data1_trn$soil), max(data1_trn$soil), length.out = 5), 0))



## Create an 'ice' object for the predictor "CHELSA_bio18_1981.2010_V.2.1":
pima.ice_18 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio18_1981.2010_V.2.1", logodds = TRUE,
               predictfcn = function(object, newdata){ 
                 predict(object, newdata, type = "prob")
               }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_18 <- min(as.numeric(as.character(names(pima.ice_18$ice_curves[1,]))))
max_18 <- max(as.numeric(as.character(names(pima.ice_18$ice_curves[1,]))))
p_18 <- plot(pima.ice_18, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
     centered = TRUE, ylab= "partial log-odds (centered)", xaxt  = "n",
     xlab= "Precipitation of warmest quarter (mm)", bty="l", las = 1, ylim = c(-1, 1.5), line_col = "yellow") 
axis(1, at=seq(min_18, max_18, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio18_1981.2010_V.2.1), max(data1_trn$CHELSA_bio18_1981.2010_V.2.1), length.out = 5), 0))


## Create an 'ice' object for the predictor "CHELSA_bio14_1981.2010_V.2.1":
pima.ice_14 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio14_1981.2010_V.2.1", logodds = TRUE,
               predictfcn = function(object, newdata){ 
                 predict(object, newdata, type = "prob")
               }
)

min_14 <- min(as.numeric(as.character(names(pima.ice_14$ice_curves[1,]))))
max_14 <- max(as.numeric(as.character(names(pima.ice_14$ice_curves[1,]))))
p_14 <- plot(pima.ice_18, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
             centered = TRUE, ylab= "partial log-odds (centered)", xaxt  = "n",
             xlab= "Precipitation of driest month (mm)", bty="l", las = 1, ylim = c(-1, 1.5)) 
axis(1, at=seq(min_14, max_14, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio14_1981.2010_V.2.1), max(data1_trn$CHELSA_bio14_1981.2010_V.2.1), length.out = 5), 0))




## Create an 'ice' object for the predictor "CHELSA_bio8_1981.2010_V.2.1":
pima.ice_8 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio8_1981.2010_V.2.1", logodds = TRUE,
                  predictfcn = function(object, newdata){ 
                    predict(object, newdata, type = "prob")
                  }
)

min_8 <- min(as.numeric(as.character(names(pima.ice_8$ice_curves[1,]))))
max_8 <- max(as.numeric(as.character(names(pima.ice_8$ice_curves[1,]))))
p_8 <- plot(pima.ice_8, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
            centered = TRUE, ylab= "partial log-odds (centered)", xaxt  = "n",
            xlab= "Mean temperature of wettest quarter (\u00B0C)", bty="l", las = 1, ylim = c(-1, 1.5)) 
axis(1, at=seq(min_8, max_8, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio8_1981.2010_V.2.1), max(data1_trn$CHELSA_bio8_1981.2010_V.2.1), length.out = 5), 0))




## Create an 'ice' object for the predictor "CHELSA_bio9_1981.2010_V.2.1":
pima.ice_9 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio9_1981.2010_V.2.1", logodds = TRUE,
                 predictfcn = function(object, newdata){ 
                   predict(object, newdata, type = "prob")
                 }
)

min_9 <- min(as.numeric(as.character(names(pima.ice_9$ice_curves[1,]))))
max_9 <- max(as.numeric(as.character(names(pima.ice_9$ice_curves[1,]))))
p_9 <- plot(pima.ice_9, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
            centered = TRUE, ylab= "partial log-odds (centered)", xaxt  = "n",
            xlab= "Mean temperature of driest quarter (\u00B0C)", bty="l", las = 1, ylim = c(-1, 1.5)) 
axis(1, at=seq(min_9, max_9, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio9_1981.2010_V.2.1), max(data1_trn$CHELSA_bio9_1981.2010_V.2.1), length.out = 5), 0))

## Create an 'ice' object for the predictor "CHELSA_bio3_1981.2010_V.2.1":
pima.ice_3 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio3_1981.2010_V.2.1", logodds = TRUE,
                  predictfcn = function(object, newdata){ 
                    predict(object, newdata, type = "prob")
                  }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_3 <- min(as.numeric(as.character(names(pima.ice_3$ice_curves[1,]))))
max_3 <- max(as.numeric(as.character(names(pima.ice_3$ice_curves[1,]))))
p_3 <- plot(pima.ice_3, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Isothermality ((BIO2/BIO7) *100)", bty="l", las = 1,  xaxt = "n") 
axis(1, at=seq(min_3, max_3, length.out= 5), labels=round(seq(min(data1_trn$CHELSA_bio3_1981.2010_V.2.1), max(data1_trn$CHELSA_bio3_1981.2010_V.2.1), length.out = 5), 0))

## Create an 'ice' object for the predictor "CHELSA_bio14_1981.2010_V.2.1":
pima.ice_14 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio14_1981.2010_V.2.1", logodds = TRUE,
               predictfcn = function(object, newdata){ 
                 predict(object, newdata, type = "prob")
               }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_14 <- min(as.numeric(as.character(names(pima.ice_14$ice_curves[1,]))))
max_14 <- max(as.numeric(as.character(names(pima.ice_14$ice_curves[1,]))))
p_14 <- plot(pima.ice_14, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Precipitation of driest month (mm)", bty="l", las = 1,  xaxt = "n") 
axis(1, at=seq(min_14, max_14, length.out= 5), labels=round(seq(min(data1_trn$CHELSA_bio14_1981.2010_V.2.1), max(data1_trn$CHELSA_bio14_1981.2010_V.2.1), length.out = 5), 0))


## Create an 'ice' object for the predictor "CHELSA_bio15_1981.2010_V.2.1":
pima.ice_15 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio15_1981.2010_V.2.1", logodds = TRUE,
               predictfcn = function(object, newdata){ 
                 predict(object, newdata, type = "prob")
               }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_15 <- min(as.numeric(as.character(names(pima.ice_15$ice_curves[1,]))))
max_15 <- max(as.numeric(as.character(names(pima.ice_15$ice_curves[1,]))))
p_15 <- plot(pima.ice_15, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Precipitation seasonality (CV)", bty="l", las = 1,  xaxt = "n") 
axis(1, at=seq(min_15, max_15, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio15_1981.2010_V.2.1), max(data1_trn$CHELSA_bio15_1981.2010_V.2.1), length.out = 5), 1))


## Create an 'ice' object for the predictor "CHELSA_bio2_1981.2010_V.2.1":
pima.ice_2 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio2_1981.2010_V.2.1", logodds = TRUE,
               predictfcn = function(object, newdata){ 
                 predict(object, newdata, type = "prob")
               }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_2 <- min(as.numeric(as.character(names(pima.ice_2$ice_curves[1,]))))
max_2 <- max(as.numeric(as.character(names(pima.ice_2$ice_curves[1,]))))
p_2 <- plot(pima.ice_2, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Mean diurnal range (\u00B0C)", bty="l", las = 1, xaxt = "n") 
axis(1, at=seq(min_2, max_2, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio2_1981.2010_V.2.1), max(data1_trn$CHELSA_bio2_1981.2010_V.2.1), length.out = 5), 1))

## Create an 'ice' object for the predictor "CHELSA_bio13_1981.2010_V.2.1":
pima.ice_13 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio13_1981.2010_V.2.1", logodds = TRUE,
                 predictfcn = function(object, newdata){ 
                   predict(object, newdata, type = "prob")
                 }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_13 <- min(as.numeric(as.character(names(pima.ice_13$ice_curves[1,]))))
max_13 <- max(as.numeric(as.character(names(pima.ice_13$ice_curves[1,]))))
p_13 <- plot(pima.ice_13, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Precipitation of wettest month (mm)", bty="l", las = 1, xaxt = "n") 
axis(1, at=seq(min_13, max_13, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio13_1981.2010_V.2.1), max(data1_trn$CHELSA_bio13_1981.2010_V.2.1), length.out = 5), 1))


## Create an 'ice' object for the predictor "CHELSA_bio19_1981.2010_V.2.1":
pima.ice_19 = ice(object = greedy_ensemble, X = X, predictor = "CHELSA_bio19_1981.2010_V.2.1", logodds = TRUE,
                  predictfcn = function(object, newdata){ 
                    predict(object, newdata, type = "prob")
                  }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_19 <- min(as.numeric(as.character(names(pima.ice_19$ice_curves[1,]))))
max_19 <- max(as.numeric(as.character(names(pima.ice_19$ice_curves[1,]))))
p_19 <- plot(pima.ice_19, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Precipitation of coldest quarter (mm)", bty="l", las = 1, xaxt = "n") 
axis(1, at=seq(min_19, max_19, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio19_1981.2010_V.2.1), max(data1_trn$CHELSA_bio19_1981.2010_V.2.1), length.out = 5), 1))


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "Figure_5_ICE_plots.jpeg", width = 2000, height = 2700,
     pointsize = 12, quality = 100, bg = "white", res = 300)
par(mfrow=c(4, 3))
p_soil <- plot(pima.ice_soil, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
             centered = TRUE, ylab= "partial log-odds (centered)", ylim = c(-1, 1.5),xaxt  = "n",
             xlab= "% clay (15-30 cm)", bty="l", las = 1) 
axis(1, at=seq(min_soil, max_soil, length.out = 5), labels=round(seq(min(data1_trn$soil)/10, max(data1_trn$soil)/10, length.out = 5), -1))
title("(a)", adj  = 0)

p_18 <- plot(pima.ice_18, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
             centered = TRUE, ylab= "partial log-odds (centered)", ylim = c(-1, 1.5),xaxt  = "n",
             xlab= "Precipitation of warmest quarter (mm)", bty="l", las = 1) 
axis(1, at=seq(min_18, max_18, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio18_1981.2010_V.2.1)*0.1, max(data1_trn$CHELSA_bio18_1981.2010_V.2.1)*0.1, length.out = 5), -1))
title("(b)", adj  = 0)

p_3 <- plot(pima.ice_3, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Isothermality ((BIO2/BIO7)*100)", bty="l", ylim = c(-1, 1.5),las = 1,  xaxt = "n") 
axis(1, at=seq(min_3, max_3, length.out= 5), labels=round(seq(min(data1_trn$CHELSA_bio3_1981.2010_V.2.1)*0.1, max(data1_trn$CHELSA_bio3_1981.2010_V.2.1)*0.1, length.out = 5), -1))
title("(c)", adj  = 0)

p_14 <- plot(pima.ice_14, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xaxt = "n", xlab= "Precipitation of driest month (mm)",  ylim = c(-1, 1.5),bty="l", las = 1) 
axis(1, at=seq(min_14, max_14, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio14_1981.2010_V.2.1)*0.1, max(data1_trn$CHELSA_bio14_1981.2010_V.2.1)*0.1, length.out = 5), 1))
title("(d)", adj  = 0)

p_13 <- plot(pima.ice_13, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Precipitation of wettest month (mm)",  ylim = c(-1, 1.5),bty="l", las = 1, xaxt = "n") 
axis(1, at=seq(min_13, max_13, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio13_1981.2010_V.2.1)*0.1, max(data1_trn$CHELSA_bio13_1981.2010_V.2.1)*0.1, length.out = 5), 0))
title("(e)", adj  = 0)

p_15 <- plot(pima.ice_15, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Precipitation seasonality (CV)", bty="l", ylim = c(-1, 1.5),las = 1,  xaxt = "n") 
axis(1, at=seq(min_15, max_15, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio15_1981.2010_V.2.1)*0.1, max(data1_trn$CHELSA_bio15_1981.2010_V.2.1)*0.1, length.out = 5), 0))
title("(f)", adj  = 0)

p_2 <- plot(pima.ice_2, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Mean diurnal range (\u00B0C)", bty="l", ylim = c(-1, 1.5),las = 1, xaxt = "n") 
axis(1, at=seq(min_2, max_2, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio2_1981.2010_V.2.1)*0.1 , max(data1_trn$CHELSA_bio2_1981.2010_V.2.1)*0.1, length.out = 5), 1))
title("(g)", adj  = 0)

p_8 <- plot(pima.ice_8, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Mean temperature of wettest quarter (\u00B0C)", bty="l", ylim = c(-1, 1.5),las = 1, xaxt = "n") 
axis(1, at=seq(min_2, max_2, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio8_1981.2010_V.2.1)*0.1 -273.15, max(data1_trn$CHELSA_bio8_1981.2010_V.2.1)*0.1 -273.15, length.out = 5), 1))
title("(h)", adj  = 0)

p_9 <- plot(pima.ice_9, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Mean temperature of driest quarter (\u00B0C)", bty="l", ylim = c(-1, 1.5),las = 1, xaxt = "n") 
axis(1, at=seq(min_9, max_9, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio9_1981.2010_V.2.1)*0.1 -273.15, max(data1_trn$CHELSA_bio9_1981.2010_V.2.1)*0.1 -273.15, length.out = 5), 1))
title("(i)", adj  = 0)

p_19 <- plot(pima.ice_19, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Precipitation of coldest quarter (mm)", bty="l", ylim = c(-1, 1.5),las = 1, xaxt = "n") 
axis(1, at=seq(min_19, max_19, length.out = 5), labels=round(seq(min(data1_trn$CHELSA_bio19_1981.2010_V.2.1)*0.1 , max(data1_trn$CHELSA_bio19_1981.2010_V.2.1)*0.1 , length.out = 5), 1))
title("(j)", adj  = 0)

dev.off()

