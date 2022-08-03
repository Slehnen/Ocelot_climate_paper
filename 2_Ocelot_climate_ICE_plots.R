require(ICEbox)
require(randomForest)
require(MASS) 

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate.RDS")
data1_trn <- readRDS("training_data_ocelot_climate.RDS")

y <- trainTransformed$type
X <- trainTransformed
X$type <- NULL


## import model
setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate.RDS")

## Create an 'ice' object for the predictor "wc2.1_2.5m_bio_18":
pima.ice_18 = ice(object = greedy_ensemble, X = X, predictor = "wc2.1_2.5m_bio_18", logodds = TRUE,
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
     xlab= "Precipitation of warmest quarter (mm)", bty="l", las = 1, ylim = c(-1, 1.5)) 
axis(1, at=seq(min_18, max_18, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_18), max(data1_trn$wc2.1_2.5m_bio_18), length.out = 5), 0))


## Create an 'ice' object for the predictor "wc2.1_2.5m_bio_8":
pima.ice_8 = ice(object = greedy_ensemble, X = X, predictor = "wc2.1_2.5m_bio_8", logodds = TRUE,
               predictfcn = function(object, newdata){ 
                 predict(object, newdata, type = "prob")
               }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_8 <- min(as.numeric(as.character(names(pima.ice_8$ice_curves[1,]))))
max_8 <- max(as.numeric(as.character(names(pima.ice_8$ice_curves[1,]))))
p_8 <- plot(pima.ice_8, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xaxt = "n", xlab= "Mean temperature of wettest quarter",  bty="l", las = 1) 
axis(1, at=seq(min_8, max_8, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_8), max(data1_trn$wc2.1_2.5m_bio_8), length.out = 5), 1))

## Create an 'ice' object for the predictor "wc2.1_2.5m_bio_3":
pima.ice_3 = ice(object = greedy_ensemble, X = X, predictor = "wc2.1_2.5m_bio_3", logodds = TRUE,
                  predictfcn = function(object, newdata){ 
                    predict(object, newdata, type = "prob")
                  }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_3 <- min(as.numeric(as.character(names(pima.ice_3$ice_curves[1,]))))
max_3 <- max(as.numeric(as.character(names(pima.ice_3$ice_curves[1,]))))
p_3 <- plot(pima.ice_3, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Isothermality", bty="l", las = 1,  xaxt = "n") 
axis(1, at=seq(min_3, max_3, length.out= 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_3), max(data1_trn$wc2.1_2.5m_bio_3), length.out = 5), 0))

## Create an 'ice' object for the predictor "wc2.1_2.5m_bio_14":
pima.ice_14 = ice(object = greedy_ensemble, X = X, predictor = "wc2.1_2.5m_bio_14", logodds = TRUE,
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
axis(1, at=seq(min_14, max_14, length.out= 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_14), max(data1_trn$wc2.1_2.5m_bio_14), length.out = 5), 0))


## Create an 'ice' object for the predictor "wc2.1_2.5m_bio_15":
pima.ice_15 = ice(object = greedy_ensemble, X = X, predictor = "wc2.1_2.5m_bio_15", logodds = TRUE,
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
axis(1, at=seq(min_15, max_15, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_15), max(data1_trn$wc2.1_2.5m_bio_15), length.out = 5), 1))


## Create an 'ice' object for the predictor "wc2.1_2.5m_bio_2":
pima.ice_2 = ice(object = greedy_ensemble, X = X, predictor = "wc2.1_2.5m_bio_2", logodds = TRUE,
               predictfcn = function(object, newdata){ 
                 predict(object, newdata, type = "prob")
               }
)

## plot
#plot(pima.ice, x_quantile = TRUE, plot_pdp = TRUE, frac_to_build = 0.1, frac_to_plot = 1) 
min_2 <- min(as.numeric(as.character(names(pima.ice_2$ice_curves[1,]))))
max_2 <- max(as.numeric(as.character(names(pima.ice_2$ice_curves[1,]))))
p_2 <- plot(pima.ice_2, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Precipitation seasonality (CV)", bty="l", las = 1, xaxt = "n") 
axis(1, at=seq(min_2, max_2, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_2), max(data1_trn$wc2.1_2.5m_bio_2), length.out = 5), 1))

## Create an 'ice' object for the predictor "wc2.1_2.5m_bio_19":
pima.ice_19 = ice(object = greedy_ensemble, X = X, predictor = "wc2.1_2.5m_bio_19", logodds = TRUE,
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
axis(1, at=seq(min_19, max_19, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_19), max(data1_trn$wc2.1_2.5m_bio_19), length.out = 5), 1))


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg(filename = "ICE_plots.jpeg", width = 1400, height = 1650,
     pointsize = 12, quality = 100, bg = "white", res = 250)
par(mfrow=c(3, 2))
p_18 <- plot(pima.ice_18, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, 
             centered = TRUE, ylab= "partial log-odds (centered)", ylim = c(-1, 1.5),xaxt  = "n",
             xlab= "Precipitation of warmest quarter (mm)", bty="l", las = 1) 
axis(1, at=seq(min_18, max_18, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_18), max(data1_trn$wc2.1_2.5m_bio_18), length.out = 5), -1))

p_3 <- plot(pima.ice_3, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Isothermality", bty="l", ylim = c(-1, 1.5),las = 1,  xaxt = "n") 
axis(1, at=seq(min_3, max_3, length.out= 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_3), max(data1_trn$wc2.1_2.5m_bio_3), length.out = 5), -1))

p_8 <- plot(pima.ice_8, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xaxt = "n", xlab= "Mean temperature of wettest quarter (\u00B0C)",  ylim = c(-1, 1.5),bty="l", las = 1) 
axis(1, at=seq(min_8, max_8, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_8), max(data1_trn$wc2.1_2.5m_bio_8), length.out = 5), 1))

p_19 <- plot(pima.ice_19, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Precipitation of coldest quarter (mm)",  ylim = c(-1, 1.5),bty="l", las = 1, xaxt = "n") 
axis(1, at=seq(min_19, max_19, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_19), max(data1_trn$wc2.1_2.5m_bio_19), length.out = 5), 0))


p_15 <- plot(pima.ice_15, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
             centered = TRUE, xlab= "Precipitation seasonality (CV)", bty="l", ylim = c(-1, 1.5),las = 1,  xaxt = "n") 
axis(1, at=seq(min_15, max_15, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_15), max(data1_trn$wc2.1_2.5m_bio_15), length.out = 5), 0))

p_2 <- plot(pima.ice_2, x_quantile = FALSE, plot_pdp = TRUE, frac_to_plot = 1, ylab= "partial log-odds (centered)",
            centered = TRUE, xlab= "Mean diurnal range (\u00B0C)", bty="l", ylim = c(-1, 1.5),las = 1, xaxt = "n") 
axis(1, at=seq(min_2, max_2, length.out = 5), labels=round(seq(min(data1_trn$wc2.1_2.5m_bio_2), max(data1_trn$wc2.1_2.5m_bio_2), length.out = 5), 1))


dev.off()

