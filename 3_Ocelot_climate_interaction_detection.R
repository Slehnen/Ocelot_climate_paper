library(iml)
library(caret)
library(gbm)
library("ggplot2")
library(mefa)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(ggpubr)


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate.RDS")

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate.RDS")
data1_trn <- readRDS("training_data_ocelot_climate.RDS")
X <- testTransformed[which(names(testTransformed) != "type")]

predictor_veg <- Predictor$new(greedy_ensemble, data = X, y = testTransformed$type)

c("wc2.1_2.5m_bio_3",  "wc2.1_2.5m_bio_19", "wc2.1_2.5m_bio_2", 
  "wc2.1_2.5m_bio_15",  "wc2.1_2.5m_bio_8",  "wc2.1_2.5m_bio_18")
# precip warmest quarter
overall <- Interaction$new(predictor_veg,  grid.size = 30)
overall <- overall
plot(overall)

int_overall <- overall$results
int_overall <- subset(int_overall, .class == "ocelot")
int_overall$var_names <- c("Precipitation of wettest month",
                           "Precipitation of driest month",
                           "Precipitation seasonality (CV)",
                           "Precipitation of warmest quarter",
                           "Precipitation of coldest quarter",
                           "Mean diurnal range",
                           "Isothermality",
                           "Mean temperature of wettest quarter",
                           "Mean temperature of driest quarter"
                           )



var_int <- ggplot(int_overall, aes(x=reorder(var_names, .interaction), y=.interaction)) + 
  geom_point() +
  geom_segment(aes(x=var_names,xend=var_names,y=0,yend=.interaction)) +
  scale_color_discrete(name="Variable Group") +
  ylab("Interaction effect (H-statistic)") +
  xlab("") +
  theme_few(15)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_flip()

var_int

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg(filename = "variable_interation.jpeg", width = 1200, height = 1200,
     pointsize = 12, quality = 100, bg = "white", res = 200)
var_int
dev.off()

# Look at interactions among three variables with higher importance
range <- Interaction$new(predictor_veg, feature = "wc2.1_2.5m_bio_2", grid.size = 30)
plot(range)

warm <- Interaction$new(predictor_veg, feature = "wc2.1_2.5m_bio_18", grid.size = 30)
plot(warm)

iso <- Interaction$new(predictor_veg, feature = "wc2.1_2.5m_bio_3", grid.size = 30)
plot(iso)

## Plot H stat of 0.35 or higher of 3 variables with higest importance
#wc2.1_2.5m_bio_8:wc2.1_2.5m_bio_2
#wc2.1_2.5m_bio_13:wc2.1_2.5m_bio_2 
#wc2.1_2.5m_bio_2:wc2.1_2.5m_bio_18
#wc2.1_2.5m_bio_19:wc2.1_2.5m_bio_3
#wc2.1_2.5m_bio_15:wc2.1_2.5m_bio_3

###################################################################
######## 2 and 8 ################################################
######## Mean diurnal range and mean temp of wettest quarter #########
###################################################################

tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$wc2.1_2.5m_bio_2  <- rep(seq(min(trainTransformed$wc2.1_2.5m_bio_2), max(trainTransformed$wc2.1_2.5m_bio_2), length.out =100), 3)  
newdat$wc2.1_2.5m_bio_8  <- rep(c(quantile(trainTransformed$wc2.1_2.5m_bio_8, 0.05), quantile(trainTransformed$wc2.1_2.5m_bio_8, 0.5), quantile(trainTransformed$wc2.1_2.5m_bio_8, 0.9)), each =100)  
newdat$temp <- factor(rep(c("17 \u00B0C", "26 \u00B0C", "28 \u00B0C"), each =100))

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- c("#00aedb", "#00b159", "#d11141")
newdat$temp <- factor(newdat$temp, levels = c("17 \u00B0C", "26 \u00B0C", "28 \u00B0C"))

p_2_8 <- ggplot(newdat, aes(x = (wc2.1_2.5m_bio_2*sd(data1_trn$wc2.1_2.5m_bio_2)+mean(data1_trn$wc2.1_2.5m_bio_2)), y= use, fill = temp)) +
  geom_line()+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = temp), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = wc2.1_2.5m_bio_2, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols) + scale_color_manual(values=cols)+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(a)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Temp. wettest quarter"))+
  xlab("Mean diurnal range \u00B0C")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_2_8

###################################################################
######## 2 and 13 ################################################
######## Mean diurnal range and precip. of wettest month #########
###################################################################

tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$wc2.1_2.5m_bio_2  <- rep(seq(min(trainTransformed$wc2.1_2.5m_bio_2), max(trainTransformed$wc2.1_2.5m_bio_2), length.out =100), 3)  
newdat$wc2.1_2.5m_bio_13  <- rep(c(quantile(trainTransformed$wc2.1_2.5m_bio_13, 0.05), quantile(trainTransformed$wc2.1_2.5m_bio_13, 0.5), quantile(trainTransformed$wc2.1_2.5m_bio_13, 0.9)), each =100)  
newdat$precip_wet <- factor(rep(c("73 mm", "218 mm", "426 mm"), each =100))

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- sjPlot::sjplot_pal(palette = "metro")
newdat$precip_wet <- factor(newdat$precip_wet, levels = c("73 mm", "218 mm", "426 mm"))

p_2_13 <- ggplot(newdat, aes(x = (wc2.1_2.5m_bio_2*sd(data1_trn$wc2.1_2.5m_bio_2)+mean(data1_trn$wc2.1_2.5m_bio_2)), y= use, fill = precip_wet)) +
  geom_line()+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = precip_wet), alpha=0.4)+
  ylim(c(0, 1.3))+
#  xlim(c(0.1, 61))+
 geom_rug(data = data1_trn, aes(x = wc2.1_2.5m_bio_2, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols) + scale_color_manual(values=cols)+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(c)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Precip. wettest month"))+
  xlab("Mean diurnal range \u00B0C")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_2_13


#######################################################################
######## 2 and 18 #####################################################
######## Precip. of coldest quarter and precip. wettest month #########
#######################################################################

tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$wc2.1_2.5m_bio_2  <- rep(seq(min(trainTransformed$wc2.1_2.5m_bio_2), max(trainTransformed$wc2.1_2.5m_bio_2), length.out =100), 3)  
newdat$wc2.1_2.5m_bio_18  <- rep(c(quantile(trainTransformed$wc2.1_2.5m_bio_18, 0.05), quantile(trainTransformed$wc2.1_2.5m_bio_18, 0.5), quantile(trainTransformed$wc2.1_2.5m_bio_18, 0.9)), each =100)  
newdat$precip_warm <- factor(rep(c("144 mm", "318 mm", "673 mm"), each =100))

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- sjPlot::sjplot_pal(palette = "metro")
newdat$precip_warm <- factor(newdat$precip_warm, levels = c("144 mm", "318 mm", "673 mm"))

p_2_18 <- ggplot(newdat, aes(x = (wc2.1_2.5m_bio_2*sd(data1_trn$wc2.1_2.5m_bio_2)+mean(data1_trn$wc2.1_2.5m_bio_2)), y= use, fill = precip_warm)) +
  geom_line()+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = precip_warm), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = wc2.1_2.5m_bio_2, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols) + scale_color_manual(values=cols)+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(b)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Precip. warmest quarter"))+
  xlab("Mean diurnal range (\u00B0C)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_2_18


#wc2.1_2.5m_bio_19:wc2.1_2.5m_bio_3
#wc2.1_2.5m_bio_15:wc2.1_2.5m_bio_3
###################################################################
######## 19 and 3 #################################################
## Precip. coldest quarter and isothermality ##########
###################################################################


tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$wc2.1_2.5m_bio_19  <- rep(seq(min(trainTransformed$wc2.1_2.5m_bio_19), max(trainTransformed$wc2.1_2.5m_bio_19), length.out =100), 3)  
newdat$wc2.1_2.5m_bio_3  <- rep(c(quantile(trainTransformed$wc2.1_2.5m_bio_3, 0.05), quantile(trainTransformed$wc2.1_2.5m_bio_3, 0.5), quantile(trainTransformed$wc2.1_2.5m_bio_3, 0.9)), each =100)  
newdat$iso <- factor(rep(c("45", "65", "77"), each =100))

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- c("#4182dd", "#2d6328", "#b6411a")
newdat$iso  <- factor(newdat$iso, levels = c("45", "65", "77"))

p_19_3 <- ggplot(newdat, aes(x = (wc2.1_2.5m_bio_19*sd(data1_trn$wc2.1_2.5m_bio_19)+mean(data1_trn$wc2.1_2.5m_bio_19)), y= use, fill = iso )) +
  geom_line()+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = iso ), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = wc2.1_2.5m_bio_19, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols) + scale_color_manual(values=cols)+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(d)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Isothermality"))+
  xlab("Precip. coldest quarter (mm)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_19_3

###################################################################
######## 15 and 3 #################################################
## Precip. seasonality and isothermality ##########
###################################################################


tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$wc2.1_2.5m_bio_15  <- rep(seq(min(trainTransformed$wc2.1_2.5m_bio_15), max(trainTransformed$wc2.1_2.5m_bio_15), length.out =100), 3)  
newdat$wc2.1_2.5m_bio_3  <- rep(c(quantile(trainTransformed$wc2.1_2.5m_bio_3, 0.05), quantile(trainTransformed$wc2.1_2.5m_bio_3, 0.5), quantile(trainTransformed$wc2.1_2.5m_bio_3, 0.9)), each =100)  
newdat$iso <- rep(c("45", "65", "77"), each =100)

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- c("#4182dd", "#2d6328", "#b6411a")
newdat$iso  <- factor(newdat$iso , levels = c("45", "65", "77"))

p_15_3 <- ggplot(newdat, aes(x = (wc2.1_2.5m_bio_15*sd(data1_trn$wc2.1_2.5m_bio_15)+mean(data1_trn$wc2.1_2.5m_bio_15)), y= use, fill = iso )) +
  geom_line()+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = iso ), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = wc2.1_2.5m_bio_15, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols) + scale_color_manual(values=cols)+
  ylab("Potential habitat probability") +
  theme_few(15)+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Isothermality"))+
  xlab("Precip. seasonality (CV))")+
  ggtitle("(e)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_15_3

ggpubr::ggarrange(p_2_8, p_2_18, p_2_13, p_19_3, p_15_3, ncol=2, nrow = 3)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures")
jpeg(filename = "variable_interactions_2_way_8_4_22.jpeg", width = 3000, height = 3500,
     pointsize = 12, quality = 100, bg = "white", res = 300)
ggpubr::ggarrange(p_2_8, p_2_18, p_2_13, p_19_3, p_15_3, ncol=2, nrow = 3)
dev.off()

