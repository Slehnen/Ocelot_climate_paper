library(iml)
library(caret)
library(gbm)
library("ggplot2")
library(mefa)
library(randomForest)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(RColorBrewer)


setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Work")
preProcValues <- readRDS("preProcValues_ocelot_climate_10_31_22.RDS")
trainTransformed <- readRDS("trainTransformed_ocelot_climate_10_31_22.RDS")
testTransformed <- readRDS("testTransformed_ocelot_climate_10_31_22.RDS")
greedy_ensemble <- readRDS("greedy_ensemble_ocelot_climate_10_31_22.RDS")
data1_trn <- readRDS("training_data_ocelot_climate_10_31_22.RDS")

X <- testTransformed[which(names(testTransformed) != "type")]

predictor_veg <- Predictor$new(greedy_ensemble, data = X, y = testTransformed$type)


# precip warmest quarter
set.seed(1111)
overall <- Interaction$new(predictor_veg)
#plot(overall)

int_overall <- overall$results
int_overall <- subset(int_overall, .class == "ocelot")
int_overall$var_names <- c("Precip. of wettest month (BIO13)",
                           "Precip. of driest month (BIO14)",
                           "Precip. seasonality (CV) (BIO15)",
                           "Precip. of warmest quarter (BIO18)",
                           "Precip. of coldest quarter (BIO19)",
                           "Mean diurnal range (BIO2)",
                           "Isothermality (BIO3)",
                           "Mean temperature of wettest quarter (BIO8)",
                           "Mean temperature of driest quarter (BIO9)",
                           "% clay (15-30 cm)"
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

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "Figure_6_variable_interation.jpeg", width = 2000, height = 1200,
     pointsize = 12, quality = 100, bg = "white", res = 300)
var_int
dev.off()

# Look at interactions among three variables with higher importance
wet <- Interaction$new(predictor_veg, feature = "CHELSA_bio13_1981.2010_V.2.1", grid.size = 30)
plot(wet)

warm <- Interaction$new(predictor_veg, feature = "CHELSA_bio18_1981.2010_V.2.1", grid.size = 30)
plot(warm)

cold <- Interaction$new(predictor_veg, feature = "CHELSA_bio19_1981.2010_V.2.1", grid.size = 30)
plot(cold)

temp <- Interaction$new(predictor_veg, feature = "CHELSA_bio8_1981.2010_V.2.1", grid.size = 30)
plot(temp)
temp

## Plot H stat of 0.25 or higher of 3 variables with highest importance
# CHELSA_bio14_1981.2010_V.2.1:CHELSA_bio19_1981.2010_V.2.1
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio19_1981.2010_V.2.1
# CHELSA_bio13_1981.2010_V.2.1:CHELSA_bio18_1981.2010_V.2.1
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio18_1981.2010_V.2.1
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio13_1981.2010_V.2.1
# CHELSA_bio13_1981.2010_V.2.1:CHELSA_bio8_1981.2010_V.2.1
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio8_1981.2010_V.2.1


###################################################################
######## 14 and 19 ################################################
######## precip. dry and precip. cold #########
###################################################################

tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$CHELSA_bio19_1981.2010_V.2.1  <- rep(seq(min(trainTransformed$CHELSA_bio19_1981.2010_V.2.1), max(trainTransformed$CHELSA_bio19_1981.2010_V.2.1), length.out =100), 3)  
newdat$CHELSA_bio14_1981.2010_V.2.1  <- rep(c(quantile(trainTransformed$CHELSA_bio14_1981.2010_V.2.1, 0.05), quantile(trainTransformed$CHELSA_bio14_1981.2010_V.2.1, 0.5), quantile(trainTransformed$CHELSA_bio14_1981.2010_V.2.1, 0.9)), each =100)  
newdat$dry <- rep(round(rep(c(quantile(data1_trn$CHELSA_bio14_1981.2010_V.2.1, 0.05), quantile(data1_trn$CHELSA_bio14_1981.2010_V.2.1, 0.5), quantile(data1_trn$CHELSA_bio14_1981.2010_V.2.1, 0.95)))*0.1, 1), each =100)


predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- brewer.pal(n = 4, name = 'YlGnBu')
newdat$dry <- factor(newdat$dry)

p_19_14 <- ggplot(newdat, aes(x = (CHELSA_bio19_1981.2010_V.2.1*sd(data1_trn$CHELSA_bio19_1981.2010_V.2.1)+mean(data1_trn$CHELSA_bio19_1981.2010_V.2.1))*0.1, y= use, fill = dry)) +
  geom_line(aes(color = dry))+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = dry), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = CHELSA_bio19_1981.2010_V.2.1*0.1, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols[2:4]) + scale_color_manual(values=cols[2:4])+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(a)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Precip. driest month (mm)"),
         colour = "none")+
  xlab("Precip. coldest quarter (mm)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_19_14

###################################################################
######## 15 and 19 ################################################
######## precipitation seasonality and precip. coldest quarter #########
###################################################################
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio19_1981.2010_V.2.1


tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$CHELSA_bio15_1981.2010_V.2.1  <- rep(seq(min(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), max(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), length.out =100), 3)  
newdat$CHELSA_bio19_1981.2010_V.2.1  <- rep(c(quantile(trainTransformed$CHELSA_bio19_1981.2010_V.2.1, 0.05), quantile(trainTransformed$CHELSA_bio19_1981.2010_V.2.1, 0.5), quantile(trainTransformed$CHELSA_bio19_1981.2010_V.2.1, 0.9)), each =100)  
newdat$precip_cold <- rep(round(rep(c(quantile(data1_trn$CHELSA_bio19_1981.2010_V.2.1, 0.05), quantile(data1_trn$CHELSA_bio19_1981.2010_V.2.1, 0.5), quantile(data1_trn$CHELSA_bio19_1981.2010_V.2.1, 0.95)))*0.1, 1), each =100)


predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- brewer.pal(n = 4, name = 'YlGnBu')
newdat$precip_cold <- factor(newdat$precip_cold)

p_15_19 <- ggplot(newdat, aes(x = (CHELSA_bio15_1981.2010_V.2.1*sd(data1_trn$CHELSA_bio15_1981.2010_V.2.1)+mean(data1_trn$CHELSA_bio15_1981.2010_V.2.1))*0.1, y= use, fill = precip_cold)) +
  geom_line(aes(color = precip_cold))+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = precip_cold), alpha=0.4)+
  ylim(c(0, 1.3))+
#  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = CHELSA_bio15_1981.2010_V.2.1*0.1, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols[2:4]) + scale_color_manual(values=cols[2:4])+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(b)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Precip. coldest quarter (mm)"),
         colour = "none")+
  xlab("Precip. seasonality (CV)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_15_19


#######################################################################
######## 13 and 18 #####################################################
######## Precip. of wettest month and precip. warmest quarter #########
#######################################################################
# CHELSA_bio13_1981.2010_V.2.1:CHELSA_bio18_1981.2010_V.2.1


tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$CHELSA_bio13_1981.2010_V.2.1  <- rep(seq(min(trainTransformed$CHELSA_bio13_1981.2010_V.2.1), max(trainTransformed$CHELSA_bio13_1981.2010_V.2.1), length.out =100), 3)  
newdat$CHELSA_bio18_1981.2010_V.2.1  <- rep(c(quantile(trainTransformed$CHELSA_bio18_1981.2010_V.2.1, 0.05), quantile(trainTransformed$CHELSA_bio18_1981.2010_V.2.1, 0.5), quantile(trainTransformed$CHELSA_bio18_1981.2010_V.2.1, 0.9)), each =100)  
newdat$precip_warm <- rep(round(rep(c(quantile(data1_trn$CHELSA_bio18_1981.2010_V.2.1, 0.05), quantile(data1_trn$CHELSA_bio18_1981.2010_V.2.1, 0.5), quantile(data1_trn$CHELSA_bio18_1981.2010_V.2.1, 0.95)))*0.1, 1), each =100)


predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- brewer.pal(n = 4, name = 'YlGnBu')
newdat$precip_warm <- factor(newdat$precip_warm)

p_13_18 <- ggplot(newdat, aes(x = (CHELSA_bio13_1981.2010_V.2.1*sd(data1_trn$CHELSA_bio13_1981.2010_V.2.1)+mean(data1_trn$CHELSA_bio13_1981.2010_V.2.1))*0.1, y= use, fill = precip_warm)) +
  geom_line(aes(color = precip_warm))+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = precip_warm), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = CHELSA_bio13_1981.2010_V.2.1 *0.1, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols[2:4]) + scale_color_manual(values=cols[2:4])+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(g)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Precip. warmest quarter (mm)"),
         colour = "none")+
  xlab("Precip. of the wettest month (mm)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_13_18


###################################################################
######## 15 and 18 #################################################
## Precip. seasonality and precip warm ##########
###################################################################
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio18_1981.2010_V.2.1


tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$CHELSA_bio15_1981.2010_V.2.1  <- rep(seq(min(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), max(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), length.out =100), 3)  
newdat$CHELSA_bio18_1981.2010_V.2.1  <- rep(c(quantile(trainTransformed$CHELSA_bio18_1981.2010_V.2.1, 0.05), quantile(trainTransformed$CHELSA_bio18_1981.2010_V.2.1, 0.5), quantile(trainTransformed$CHELSA_bio18_1981.2010_V.2.1, 0.9)), each =100)  
newdat$warm <- rep(round(rep(c(quantile(data1_trn$CHELSA_bio18_1981.2010_V.2.1, 0.05), quantile(data1_trn$CHELSA_bio18_1981.2010_V.2.1, 0.5), quantile(data1_trn$CHELSA_bio18_1981.2010_V.2.1, 0.95)))*0.1, 1), each =100)

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- brewer.pal(n = 4, name = 'YlGnBu')
newdat$warm <- factor(newdat$warm)


p_15_18 <- ggplot(newdat, aes(x = (CHELSA_bio15_1981.2010_V.2.1*sd(data1_trn$CHELSA_bio15_1981.2010_V.2.1)+mean(data1_trn$CHELSA_bio15_1981.2010_V.2.1))*0.1, y= use, fill = warm )) +
  geom_line(aes(color = warm))+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = warm), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = CHELSA_bio15_1981.2010_V.2.1*0.1, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols[2:4]) + scale_color_manual(values=cols[2:4])+
  ylab("Potential habitat probability") +
  theme_few(15)+
  ggtitle("(c)")+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Precip. warmest quarter (mm)"),
         colour = "none")+
  xlab("Precip. seasonality (CV)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_15_18

###################################################################
######## 15 and 13 #################################################
## Precip. seasonality and precip. wettest month ##########
###################################################################
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio13_1981.2010_V.2.1

tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$CHELSA_bio15_1981.2010_V.2.1  <- rep(seq(min(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), max(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), length.out =100), 3)  
newdat$CHELSA_bio13_1981.2010_V.2.1  <- rep(c(quantile(trainTransformed$CHELSA_bio13_1981.2010_V.2.1, 0.05), quantile(trainTransformed$CHELSA_bio13_1981.2010_V.2.1, 0.5), quantile(trainTransformed$CHELSA_bio13_1981.2010_V.2.1, 0.95)), each =100)  
newdat$wet <- rep(round(rep(c(quantile(data1_trn$CHELSA_bio13_1981.2010_V.2.1, 0.05), quantile(data1_trn$CHELSA_bio13_1981.2010_V.2.1, 0.5), quantile(data1_trn$CHELSA_bio13_1981.2010_V.2.1, 0.95)))*0.1, 1), each =100)

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- brewer.pal(n = 4, name = 'YlGnBu')
newdat$wet  <- factor(newdat$wet)

p_15_13 <- ggplot(newdat, aes(x = (CHELSA_bio15_1981.2010_V.2.1*sd(data1_trn$CHELSA_bio15_1981.2010_V.2.1)+mean(data1_trn$CHELSA_bio15_1981.2010_V.2.1))*0.1, y= use, fill = wet)) +
  geom_line(aes(color = wet))+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = wet), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = CHELSA_bio15_1981.2010_V.2.1*0.1, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols[2:4]) + scale_color_manual(values=cols[2:4])+
  ylab("Potential habitat probability") +
  theme_few(15)+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Precip. of wettest month (mm)"),
         colour = "none")+
  xlab("Precip. seasonality (CV)")+
  ggtitle("(d)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_15_13

###################################################################
######## 13 and 8 #################################################
## Precip. seasonality and precip. wettest month ##########
###################################################################
# CHELSA_bio13_1981.2010_V.2.1:CHELSA_bio8_1981.2010_V.2.1

tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$CHELSA_bio13_1981.2010_V.2.1  <- rep(seq(min(trainTransformed$CHELSA_bio13_1981.2010_V.2.1), max(trainTransformed$CHELSA_bio13_1981.2010_V.2.1), length.out =100), 3)  
newdat$CHELSA_bio8_1981.2010_V.2.1  <- rep(c(quantile(trainTransformed$CHELSA_bio8_1981.2010_V.2.1, 0.05), quantile(trainTransformed$CHELSA_bio8_1981.2010_V.2.1, 0.5), quantile(trainTransformed$CHELSA_bio8_1981.2010_V.2.1, 0.95)), each =100)  
newdat$temp <- rep(round(rep(c(quantile(data1_trn$CHELSA_bio8_1981.2010_V.2.1, 0.05), quantile(data1_trn$CHELSA_bio8_1981.2010_V.2.1, 0.5), quantile(data1_trn$CHELSA_bio8_1981.2010_V.2.1, 0.95)))*0.1 -273.15, 1), each =100)

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- brewer.pal(n = 4, name = 'YlOrRd')
newdat$temp  <- factor(newdat$temp)

p_13_8 <- ggplot(newdat, aes(x = (CHELSA_bio13_1981.2010_V.2.1*sd(data1_trn$CHELSA_bio13_1981.2010_V.2.1)+mean(data1_trn$CHELSA_bio13_1981.2010_V.2.1))*0.1, y= use, fill = temp )) +
  geom_line(aes(color = temp))+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = temp ), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = CHELSA_bio13_1981.2010_V.2.1*0.1, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols[2:4]) + scale_color_manual(values=cols[2:4])+
  ylab("Potential habitat probability") +
  theme_few(15)+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Temp. wettest quarter (\u00B0C)"),
         colour = "none")+
  xlab("Precip. of wettest month (mm)")+
  ggtitle("(f)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_13_8

###################################################################
######## 15 and 8 #################################################
## Precip. seasonality and temp. wettest quarter ##########
###################################################################
# CHELSA_bio15_1981.2010_V.2.1:CHELSA_bio8_1981.2010_V.2.1

tr_median <- apply(within(trainTransformed, rm("type")),2,median)
newdat  <-  as.data.frame(t(tr_median))
newdat  <-  rbind(newdat, newdat[rep(1, 299), ])
newdat$CHELSA_bio15_1981.2010_V.2.1  <- rep(seq(min(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), max(trainTransformed$CHELSA_bio15_1981.2010_V.2.1), length.out =100), 3)  
newdat$CHELSA_bio8_1981.2010_V.2.1  <- rep(c(quantile(trainTransformed$CHELSA_bio8_1981.2010_V.2.1, 0.05), quantile(trainTransformed$CHELSA_bio8_1981.2010_V.2.1, 0.5), quantile(trainTransformed$CHELSA_bio8_1981.2010_V.2.1, 0.95)), each =100)  
newdat$temp <- rep(round(rep(c(quantile(data1_trn$CHELSA_bio8_1981.2010_V.2.1, 0.05), quantile(data1_trn$CHELSA_bio8_1981.2010_V.2.1, 0.5), quantile(data1_trn$CHELSA_bio8_1981.2010_V.2.1, 0.95)))*0.1 -273.15, 1), each =100)

predict_out  <-  predict(greedy_ensemble, newdat, type = "prob",  se = TRUE)
newdat$use <- predict_out$fit
newdat$use_adj <- newdat$use - newdat$use[1]
newdat$use_low  <-  predict_out$lwr
newdat$use_high  <-  predict_out$upr
library(ggplot2)
library(ggthemes)
library(sjPlot)
cols <- brewer.pal(n = 4, name = 'YlOrRd')
newdat$temp  <- factor(newdat$temp)


p_15_8 <- ggplot(newdat, aes(x = (CHELSA_bio15_1981.2010_V.2.1*sd(data1_trn$CHELSA_bio15_1981.2010_V.2.1)+mean(data1_trn$CHELSA_bio15_1981.2010_V.2.1))*0.1, y= use, fill = temp )) +
  geom_line(aes(color=temp))+
  geom_ribbon(aes(ymin = use_low, ymax = use_high, fill = temp ), alpha=0.4)+
  ylim(c(0, 1.3))+
  #  xlim(c(0.1, 61))+
  geom_rug(data = data1_trn, aes(x = CHELSA_bio15_1981.2010_V.2.1*0.1, y=0.01, fill=NULL, color=NULL), position='jitter', alpha = 0.1, sides="b")+
  scale_fill_manual(values=cols[2:4]) + scale_color_manual(values=cols[2:4])+
  ylab("Potential habitat probability") +
  theme_few(15)+
  theme(legend.position = c(0.3, 0.85))+
  guides(fill=guide_legend(title="Temp. wettest quarter (\u00B0C)"),
         colour = "none")+
  xlab("Precip. seasonality (CV)")+
  ggtitle("(e)")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
p_15_8

ggpubr::ggarrange(p_19_14, p_15_19, p_13_18, p_15_18, p_15_13, p_13_8, p_15_8, ncol=2, nrow = 4)

setwd("C:/Users/slehnen/OneDrive - DOI/Ocelot_collab/Figures/Revised_figures")
jpeg(filename = "Figure_7_variable_interactions_2_way_12_1_22.jpeg", width = 3500, height = 4800,
     pointsize = 12, quality = 100, bg = "white", res = 300)
ggpubr::ggarrange(p_19_14, p_15_19, p_15_18, p_15_13, p_15_8, p_13_8, p_13_18, ncol=2, nrow = 4)
dev.off()

