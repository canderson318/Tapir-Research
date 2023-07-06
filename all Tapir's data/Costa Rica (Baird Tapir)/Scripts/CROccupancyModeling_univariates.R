###------------------------------------------------------------------------------###
### Running models on baird's
### Edited 2023-6-29 by Christian 
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)")
library(unmarked)

tapir_t<- readRDS("scripts/tapir_CR.rds")
head(tapir_t)
eff_t<- readRDS("scripts/eff_CR.rds")
head(eff_t)
cv_t3<- read.csv("cv_t3.csv")
head(cv_t3)
cv_t3<- cbind(cv_t3[,2:5], round(scale(cv_t3[,6:ncol(cv_t3)]),3))

rownames(tapir_t) == rownames(eff_t)
rownames(eff_t) == cv_t3$Station

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=cv_t3, obsCovs=list(Eff=eff_t))
summary(umf)

#-----------------------------------------------------------------------
# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
#plogis( -0.129)  	# Probability of occupancy
#plogis(  -0.832)	# Probability of detection
#22/141	#Naive occupancy

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
#summary(m.psi1.pEff)

# Running unicovariate models
m.psiElev.pEff<- occu(~Eff~Elev , umf)	# SIGNIFICANT!!
# summary(m.psiElev.pEff)
# plogis(0.000888) #Occupancy Estimate= 0.500222
# plogis(0.0104) #Detection Estimate= 0.5026
m.psiHFI.pEff<- occu(~Eff~HFI , umf) 	# SIGNIFICANT!!
#summary(m.psiHFI.pEff)
m.psiFor.pEff<- occu(~Eff~Forest , umf)
#summary(m.psiFor.pEff)
m.psiNPP.pEff<- occu(~Eff~NPP , umf)
#summary(m.psiNPP.pEff)
m.psiRoad.pEff<- occu(~Eff~d.Road , umf)	# SIGNIFICANT!!
#summary(m.psiRoad.pEff)
m.psiRiver.pEff<- occu(~Eff~d.River , umf)
#summary(m.psiRiver.pEff)
m.psiED.pEff<- occu(~Eff~EdgeDens , umf)
#summary(m.psiED.pEff)
m.psiPD.pEff<- occu(~Eff~PatchDens , umf)
#summary(m.psiPD.pEff)
m.psiDC.pEff<- occu(~Eff~DisjCore , umf)
#summary(m.psiDC.pEff)
m.psiTempmin.pEff<- occu(~Eff~ Avg.Min.Temp, umf) ####SOUTH SIGNIFICANT!!######
#summary(m.psiTempmin.pEff)
m.psiTempmax.pEff<- occu(~Eff~ Avg.Max.Temp, umf) ####SOUTH SIGNIFICANT!!######
#summary(m.psiTempmax.pEff)
#plogis( -0.0803) #Occupancy Estimate= 0.4799358
#plogis(-0.0504) #Detection Estimate= 0.4874027
m.psiNDVI.pEff<- occu(~Eff~ NDVI, umf) ####SOUTH SIGNIFICANT!!######
#summary(m.psiNDVI.pEff)
#~Eff ~Precipitaion
m.pEff.psiPrec<- occu(~Eff ~Precip, umf)
summary(m.pEff.psiPrec)

##collect in fitList
detList<-fitList(mod0, 
                 m.psi1.pEff,
                 m.psiElev.pEff, 
              		m.psiHFI.pEff,
              		m.psiFor.pEff,
              		m.psiNPP.pEff,
              		m.psiRoad.pEff,
              		m.psiRiver.pEff,
              		m.psiED.pEff,
              		m.psiPD.pEff,
              		m.psiDC.pEff,
              		m.psiTempmin.pEff, 
              		m.psiTempmax.pEff,
                 m.psiNDVI.pEff)

##do AIC model selection
modSel(detList) 

summary(m.psiRoad.pEff)     
summary(m.psiReg.pEff)       
summary(m.psiHFI.pEff)      
summary(m.psiElev.pEff)

# Plotting top Unicovariate model (Roads)
hist(sort(scale(sc_t2$d.Road)))
#pdf("Tapir_unicovRoad_CRtotal.pdf")
pred.psi.road<- predict(m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(sc_t2$d.Road))), "state")
plot(sort(scale(sc_t2$d.Road)), pred.psi.road$Predicted, type="l", xaxt="n", 
    cex.lab=1.0, main = "Distance to Road and Baird's Tapir Occupancy",xlab="Distance to roads (d.Road)", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, seq(-1,10,1))
points(sort(scale(sc_t2$d.Road)), pred.psi.road$lower, type="l", lty=2)
points(sort(scale(sc_t2$d.Road)), pred.psi.road$upper, type="l", lty=2)
#dev.off() 

# Plotting top Unicovariate model (HFI)
hist(sort(scale(sc_t$HFI)))
#pdf("Tapir_unicovHFI_CRtotal.pdf")
pred.psi.hfi<- predict(m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(sc_t$HFI))), "state")
plot(sort(scale(sc_t$HFI)), pred.psi.hfi$Predicted, type="l", xaxt="n", 
     cex.lab=1.3, main = "HFI and Baird's Tapir Occupancy", xlab="Human Footprint Index (HFI)", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t$HFI)), labels = round(sort(sc_t$HFI),0))
####axis(1, at=c(11, 15, 17), cex.axis = 1) --> Ask Dr. Rocha!!
points(sort(scale(sc_t$HFI)), pred.psi.hfi$lower, type="l", lty=2)
points(sort(scale(sc_t$HFI)), pred.psi.hfi$upper, type="l", lty=2)
#dev.off()

# Plotting top Unicovariate model (Elev)
hist(sort(scale(sc_t$Elev)))
#pdf("Tapir_unicovElev_CRtotal.pdf")
pred.psi.elev<- predict(m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(sc_t$Elev))), "state")
plot(sort(scale(sc_t$Elev)), pred.psi.elev$Predicted, type="l", xaxt="n", 
     cex.lab=1.3, main = "Elev and Baird's Tapir Occupancy", xlab="Elevation ", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t$Elev)), labels = round(sort(sc_t$HFI),0))
####axis(1, at=c(11, 15, 17), cex.axis = 1) --> Ask Dr. Rocha!!
points(sort(scale(sc_t$Elev)), pred.psi.elev$lower, type="l", lty=2)
points(sort(scale(sc_t$Elev)), pred.psi.elev$upper, type="l", lty=2)
#dev.off()

#pdf(file = "CR_tapir_psiReg.pdf", 9, 5, paper = "USr")
pred.psi.reg<- predict(m.psiReg.pEff, newdata= data.frame(Dataset= sort(unique(sc_t$Dataset))), "state")
plot(1:4, pred.psi.reg$Predicted, xaxt="n", cex.lab=1.3, pch=19, cex=1.3, main = "Probability of Occupancy for Baird's Tapir in Costa Rica by Region",
     xlab="Regions", ylab="Probability of Occupancy", col="blue", ylim= c(0,1),
     xlim= c(0.5,4.5))
arrows(1:4, pred.psi.reg$lower, 1:4, pred.psi.reg$upper, code = 3, angle = 90,
       length = 0.05)
axis(side = 1, at = 1:4, labels = sort(unique(sc_t$Dataset)))
#dev.off()

#combining all graphs into one chart

#Data
set.seed(6)
x <- rexp(50)

# Three rows, two columns
par(mfrow = c(2, 2))

# Plots-------------------------
plot(sort(scale(sc_t2$d.Road)), pred.psi.road$Predicted, type="l", xaxt="n", 
     cex.lab=1, cex.main = 1, main = "Distance to Road and Baird's Tapir Occupancy",xlab="Distance to roads (d.Road)", ylab="Probability of Occupancy", ylim= c(0.15,1))
axis(side = 1, seq(-1,10,1))
points(sort(scale(sc_t2$d.Road)), pred.psi.road$lower, type="l", lty=2, col="blue")
points(sort(scale(sc_t2$d.Road)), pred.psi.road$upper, type="l", lty=2, col="blue") # Top left
plot(sort(scale(sc_t$HFI)), pred.psi.hfi$Predicted, type="l", xaxt="n", cex.lab=1, cex.main = 1, main = "HFI and Baird's Tapir Occupancy", xlab="Human Footprint Index (HFI)", ylab="Probability of Occupancy", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t$HFI)), labels = round(sort(sc_t$HFI),0))
####axis(1, at=c(11, 15, 17), cex.axis = 1) --> Ask Dr. Rocha!!
points(sort(scale(sc_t$HFI)), pred.psi.hfi$lower, type="l", lty=2, col="blue")
points(sort(scale(sc_t$HFI)), pred.psi.hfi$upper, type="l", lty=2, col="blue") # Top right
plot(sort(scale(sc_t$Elev)), pred.psi.elev$Predicted, type="l", xaxt="n", 
     cex.lab=1, cex.main = 1, main = "Elevation and Baird's Tapir Occupancy", xlab="Elevation ", ylab="Probability of Occupancy", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t$Elev)), labels = round(sort(sc_t$HFI),0))
####axis(1, at=c(11, 15, 17), cex.axis = 1) --> Ask Dr. Rocha!!
points(sort(scale(sc_t$Elev)), pred.psi.elev$lower, type="l", lty=2, col="blue")
points(sort(scale(sc_t$Elev)), pred.psi.elev$upper, type="l", lty=2, col="blue") # Bottom left

#-----------------------------

# Back to the original graphics device
par(mfrow = c(1, 1))


########################################################################################################
### Multicovariate models
# You can see that Road, Region, HFI, Elev performed better than the null model (m.psi1.pEff)
# Next step: construct multivariate models with all possible combinations of Road, Region, HFI and Elev . 
# Check what the best performing model is based on AIC values (the same way you did for the unicovariate models)
#Eff, Elev + HFI
#Eff, Elev + Road
#Eff, Elev + Region
#Eff, HFI + Road
#Eff, HFI + Region
#Eff, Road + Region

#Two Variables 
m.psiElevTempmax.pEff<- occu(~Eff~ Elev + Avg.Max.Temp, umf)	#Elevation significant!
summary(m.psiElevTempmax.pEff)   
m.psiElevPrecip.pEff<- occu(~Eff~ Elev + Precip, umf)	#Elevation significant!
summary(m.psiElevPrecip.pEff)  
plogis(0.000683) #Occupancy Estimate (Elev) = 0.5001707
plogis( 0.004718) #Occupancy Estimate (Precip) = 0.5011795
(0.5001707+0.5011795)/2  # = 0.5006751!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plogis( -0.0331) #Detection Estimate= 0.4917258
m.psiElevRoad.pEff<- occu(~Eff~ Elev + d.Road, umf)	#Elevation significant!
summary(m.psiElevRoad.pEff) 
m.psiElevNDVI.pEff<- occu(~Eff~ Elev + NDVI, umf)	#Elevation significant!
summary(m.psiElevNDVI.pEff)  
m.psiAvgMaxTempPrecip.pEff<- occu(~Eff~ Avg.Max.Temp + Precip, umf)	#Elevation significant!
summary(m.psiAvgMaxTempPrecip.pEff)  
plogis(-0.1298 ) #Occupancy Estimate (Temp) = 0.4675955
plogis( 0.0131) #Occupancy Estimate (Precip) = 0.503275
(0.4675955+0.503275)/2  # = 0.4854352!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plogis(-0.050) #Detection Estimate= 0.4875026
m.psiAvgMaxTempRoad.pEff<- occu(~Eff~ Avg.Max.Temp + d.Road, umf)	#Elevation significant!
summary(m.psiAvgMaxTempRoad.pEff)  
m.psiAvgMaxTempNDVI.pEff<- occu(~Eff~ Avg.Max.Temp + NDVI, umf)	#Elevation significant!
summary(m.psiAvgMaxTempNDVI.pEff)  
m.psiPrecipRoad.pEff<- occu(~Eff~ Precip + d.Road, umf)	#Elevation significant!
summary(m.psiPrecipRoad.pEff)  
m.psiPrecipNDVI.pEff<- occu(~Eff~ Precip + NDVI, umf)	#Elevation significant!
summary(m.psiPrecipNDVI.pEff)  
m.psiNDVIRoad.pEff<- occu(~Eff~ NDVI + d.Road, umf)	#Elevation significant!
summary(m.psiNDVIRoad.pEff)  

#Three Variables
#Eff, Elev + AvgMaxTemp + Precip
m.psiElevTempmaxPrecip.pEff<- occu(~Eff~ Elev + Avg.Max.Temp + Precip, umf)	#Elevation significant!
summary(m.psiElevTempmaxPrecip.pEff) 
#Eff, Elev + AvgMaxTemp + d.Road
m.psiElevTempmaxRoad.pEff<- occu(~Eff~ Elev + Avg.Max.Temp + d.Road, umf)	#Elevation significant!
summary(m.psiElevTempmaxRoad.pEff) 
#Eff, Elev + AvgMaxTemp + NDVI
m.psiElevTempmaxNDVI.pEff<- occu(~Eff~ Elev + Avg.Max.Temp + NDVI, umf)	#Elevation significant!
summary(m.psiElevTempmaxNDVI.pEff) 
#Eff, AvgMaxTime + Precip + d.Road
m.psiTempmaxPrecipRoad.pEff<- occu(~Eff~ Avg.Max.Temp + Precip + d.Road, umf)	#Elevation significant!
summary(m.psiTempmaxPrecipRoad.pEff) 
#Eff, AvgMaxTemp + Precip + NDVI
m.psiAvgMaxTempPrecipNDVI.pEff<- occu(~Eff~ Avg.Max.Temp + Precip + NDVI, umf)	#Elevation significant!
summary(m.psiAvgMaxTempPrecipNDVI.pEff) 
plogis(-0.1201) #Occupancy Estimate (Temp) = 0.470011
plogis(0.0136) #Occupancy Estimate (Precip) = 0.5033999
plogis(-2.0362) #Occupancy Estimate (NDVI) = 0.1154542
(0.470011+0.5033999+0.1154542)/3  # = 0.362955!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plogis(-0.0502) #Detection Estimate= 0.4874526
#Eff, AvgMaxTemp + NDVI + d.Road
m.psiAvgMaxTempNDVIRoad.pEff<- occu(~Eff~ Avg.Max.Temp + NDVI + d.Road, umf)	#Elevation significant!
summary(m.psiAvgMaxTempNDVIRoad.pEff) 
#Eff, Precip + d.Road + NDVI
m.psiPrecipRoadNDVI.pEff<- occu(~Eff~ Precip + d.Road + NDVI, umf)	#Elevation significant!
summary(m.psiPrecipRoadNDVI.pEff) 
#Eff, Precip + Elev + NDVI
m.psiPrecipElevNDVI.pEff<- occu(~Eff~ Precip + Elev + NDVI, umf)	#Elevation significant!
summary(m.psiPrecipElevNDVI.pEff) 
plogis(0.005669) #Occupancy Estimate (Precip) =  0.5014172
plogis(0.000685) #Occupancy Estimate (Elev) = 0.5001712
plogis(-0.677362) #Occupancy Estimate (NDVI) = 0.3368503
(0.5014172+0.5001712+0.3368503)/3  # = 0.4461462!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plogis(-0.0365) #Detection Estimate= 0.490876
#Eff, Precip + Elev + d.Road
m.psiPrecipElevRoad.pEff<- occu(~Eff~ Precip + Elev + d.Road, umf)	#Elevation significant!
summary(m.psiPrecipElevRoad.pEff) 
#Eff, d.Road + NDVI + Elev
m.psiRoadNDVIElev.pEff<- occu(~Eff~ d.Road + NDVI + Elev, umf)	#Elevation significant!
summary(m.psiRoadNDVIElev.pEff) 

#Four Variables
m.psiTempElevNDVIRoad.pEff <- occu(~Eff~ Avg.Max.Temp + Elev + NDVI + d.Road, umf)	#Elevation significant!
summary(m.psiTempElevNDVIRoad.pEff)        
m.psiElevNDVIPrecipRoad.pEff <- occu(~Eff~ Elev + NDVI + Precip + d.Road, umf)	#Elevation significant!
summary(m.psiElevNDVIPrecipRoad.pEff) 
m.psiTempElevRoadPrecip.pEff <- occu(~Eff~ Avg.Max.Temp + Elev + d.Road + Precip, umf)	#Elevation significant!
summary(m.psiTempElevRoadPrecip.pEff)
m.psiTempNDVIRoadPrecip.pEff <- occu(~Eff~ Avg.Max.Temp + NDVI + d.Road + Precip, umf)	#Elevation significant!
summary(m.psiTempNDVIRoadPrecip.pEff)    	 
m.psiTempElevNDVIPrecip.pEff <- occu(~Eff~ Avg.Max.Temp + Elev + NDVI + Precip, umf)	#Elevation significant!
summary(m.psiTempElevNDVIPrecip.pEff)     	 

##collect in fitList ---> all variables 
detList2<-fitList(mod0, 
                  m.pEff.psiPrec, 
                  m.psiElev.pEff, 
                  m.psiRoad.pEff, 
                  m.psiTempmax.pEff,
                  m.psiNDVI.pEff, 
                  m.psiElevRoad.pEff, 
                  m.psiElevTempmax.pEff,
                  m.psiElevPrecip.pEff,
                  m.psiElevNDVI.pEff,
                  m.psiAvgMaxTempPrecip.pEff, 
                  m.psiAvgMaxTempRoad.pEff,
                  m.psiAvgMaxTempNDVI.pEff, 
                  m.psiPrecipRoad.pEff, 
                  m.psiPrecipNDVI.pEff, 
                  m.psiNDVIRoad.pEff,
                  m.psiElevTempmaxPrecip.pEff,
                  m.psiElevTempmaxRoad.pEff, 
                  m.psiElevTempmaxNDVI.pEff,
                  m.psiTempmaxPrecipRoad.pEff,
                  m.psiAvgMaxTempPrecipNDVI.pEff,
                  m.psiAvgMaxTempNDVIRoad.pEff, 
                  m.psiPrecipRoadNDVI.pEff, 
                  m.psiPrecipElevNDVI.pEff, 
                  m.psiPrecipElevRoad.pEff, 
                  m.psiRoadNDVIElev.pEff, 
                  m.psiTempElevNDVIRoad.pEff,
                  m.psiElevNDVIPrecipRoad.pEff, 
                  m.psiTempElevRoadPrecip.pEff,
                  m.psiTempNDVIRoadPrecip.pEff, 
                  m.psiTempElevNDVIPrecip.pEff,
                  m.psi1.pEff,
                   
                  m.psiHFI.pEff,
                  m.psiFor.pEff,
                  m.psiNPP.pEff,
                  
                  m.psiRiver.pEff,
                  m.psiED.pEff,
                  m.psiPD.pEff,
                  m.psiDC.pEff,
                  m.psiTempmin.pEff)

##do AIC model selection
sink("bairdModSel.txt")
modSel(detList2) 
sink()
#Top Model (for 2 variable) according to AIC= m.psiElevRoad.pEff 
summary(m.psiElevRoad.pEff)

####################Plotting Multicovariate Models#####################################

#m.psiElevHFI.pEff
#---HFI Constant
#load new library
library(AICcmodavg)
library(dplyr)
library(ggplot2)

#create a list of models you are trying to plot
mods <- list(m.psiElevHFI.pEff = m.psiElevHFI.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_Elev_newdata <- data.frame(Elev = seq(min(scale(sc_t$Elev)), 
                                           max(scale(sc_t$Elev)), by = 0.1),
                                HFI = mean(scale(sc_t$HFI))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_Elev_pred <- modavgPred(mods,
                             # c.hat =    # to change variance inflation factor, default = 1) 
                             parm.type = "psi", # psi = occupancy
                             newdata = occu_Elev_newdata)[c("mod.avg.pred",
                                                            "lower.CL",
                                                            "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_Elev_pred_df <- data.frame(Predicted = occu_Elev_pred$mod.avg.pred,
                                lower = occu_Elev_pred$lower.CL,
                                upper = occu_Elev_pred$upper.CL,
                                occu_Elev_newdata)


plot(occu_Elev_newdata$Elev, occu_Elev_pred$mod.avg.pred, type="l", 
     xlab="Elevation", ylab="Occupancy probability", main = "Impact of Elevation on Baird's Tapir Occupancy (HFI Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_Elev_newdata$Elev,occu_Elev_pred_df$lower, type="l", lty=2, col="blue")
points(occu_Elev_newdata$Elev,occu_Elev_pred_df$upper, type="l", lty=2, col="blue")
#----Elev constant
#create a list of models you are trying to plot
mods <- list(m.psiElevHFI.pEff = m.psiElevHFI.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_HFI_newdata <- data.frame(HFI = seq(min(scale(sc_t$HFI)), 
                                           max(scale(sc_t$HFI)), by = 0.1),
                                Elev = mean(scale(sc_t$Elev))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_HFI_pred <- modavgPred(mods,
                             # c.hat =    # to change variance inflation factor, default = 1) 
                             parm.type = "psi", # psi = occupancy
                             newdata = occu_HFI_newdata)[c("mod.avg.pred",
                                                            "lower.CL",
                                                            "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_HFI_pred_df <- data.frame(Predicted = occu_HFI_pred$mod.avg.pred,
                                lower = occu_HFI_pred$lower.CL,
                                upper = occu_HFI_pred$upper.CL,
                                occu_HFI_newdata)


plot(occu_HFI_newdata$HFI, occu_HFI_pred$mod.avg.pred, type="l", 
     xlab="Human Footprint Index (HFI)", ylab="Occupancy probability", main = "Impact of Human Footprint Index (HFI) on Baird's Tapir Occupancy (Elevation Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_HFI_newdata$HFI,occu_HFI_pred_df$lower, type="l", lty=2, col="blue")
points(occu_HFI_newdata$HFI,occu_HFI_pred_df$upper, type="l", lty=2, col="blue")

#------------------------------------------------------------------------------------

#m.psiElevRoad.pEff model (d.Road constant) --->>>>>>>TOP MODEL - Include in Report
#---d.Road constant
#create a list of models you are trying to plot
mods <- list(m.psiElevRoad.pEff = m.psiElevRoad.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_ElevRoad_newdata <- data.frame(Elev = seq(min(scale(sc_t$Elev)), 
                                         max(scale(sc_t$Elev)), by = 0.1),
                               d.Road = mean(scale(sc_t$d.Road))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_ElevRoad_pred <- modavgPred(mods,
                            # c.hat =    # to change variance inflation factor, default = 1) 
                            parm.type = "psi", # psi = occupancy
                            newdata = occu_ElevRoad_newdata)[c("mod.avg.pred",
                                                          "lower.CL",
                                                          "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_ElevRoad_pred_df <- data.frame(Predicted = occu_ElevRoad_pred$mod.avg.pred,
                               lower = occu_ElevRoad_pred$lower.CL,
                               upper = occu_ElevRoad_pred$upper.CL,
                               occu_ElevRoad_newdata)


plot(occu_ElevRoad_newdata$Elev, occu_ElevRoad_pred$mod.avg.pred, type="l", 
     xlab="Elevation", ylab="Occupancy probability", main = "Impact of Elevation on Baird's Tapir Occupancy (d.Road Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_ElevRoad_newdata$Elev,occu_ElevRoad_pred_df$lower, type="l", lty=2, col="blue")
points(occu_ElevRoad_newdata$Elev,occu_ElevRoad_pred_df$upper, type="l", lty=2, col="blue")

#---Elev Constant

#create a list of models you are trying to plot
mods <- list(m.psiElevRoad.pEff = m.psiElevRoad.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_RoadElev_newdata <- data.frame(d.Road = seq(min(scale(sc_t$d.Road)), 
                                         max(scale(sc_t$d.Road)), by = 0.1),
                               Elev = mean(scale(sc_t$Elev))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_RoadElev_pred <- modavgPred(mods,
                            # c.hat =    # to change variance inflation factor, default = 1) 
                            parm.type = "psi", # psi = occupancy
                            newdata = occu_RoadElev_newdata)[c("mod.avg.pred",
                                                          "lower.CL",
                                                          "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_RoadElev_pred_df <- data.frame(Predicted = occu_RoadElev_pred$mod.avg.pred,
                               lower = occu_RoadElev_pred$lower.CL,
                               upper = occu_RoadElev_pred$upper.CL,
                               occu_RoadElev_newdata)


plot(occu_RoadElev_newdata$d.Road, occu_RoadElev_pred$mod.avg.pred, type="l", 
     xlab="Distance to Road", ylab="Occupancy probability", main = "Impact of Distance to Road on Baird's Tapir Occupancy (Elevation Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_RoadElev_newdata$d.Road,occu_RoadElev_pred_df$lower, type="l", lty=2, col="blue")
points(occu_RoadElev_newdata$d.Road,occu_RoadElev_pred_df$upper, type="l", lty=2, col="blue")

#--------------------------------------------------------------------------------
#m.psiElevReg.pEff ---> CANT GRAPH NUMERICAL WITH CATEGORICAL
#--Elev Constant
#create a list of models you are trying to plot
  mods <- list(m.psiElevReg.pEff = m.psiElevReg.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_RegElev_newdata <- data.frame(Dataset = seq(min(scale(sc_t$Dataset)), 
                                             max(scale(sc_t$Dataset)), by = 0.1),
                                Elev = mean(scale(sc_t$Elev))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_RegElev_pred <- modavgPred(mods,
                             # c.hat =    # to change variance inflation factor, default = 1) 
                             parm.type = "psi", # psi = occupancy
                             newdata = occu_RegElev_newdata)[c("mod.avg.pred",
                                                            "lower.CL",
                                                            "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_RegElev_pred_df <- data.frame(Predicted = occu_RegElev_pred$mod.avg.pred,
                                lower = occu_RegElev_pred$lower.CL,
                                upper = occu_RegElev_pred$upper.CL,
                                occu_RegElev_newdata)


plot(occu_RegElev_newdata$Dataset, occu_RegElev_pred$mod.avg.pred, type="l", 
     xlab="Region", ylab="Occupancy probability", main = "Impact of Region on Baird's Tapir Occupancy (Elevation Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_RegElev_newdata$Dataset,occu_RegElev_pred_df$lower, type="l", lty=2, col="blue")
points(occu_RegElev_newdata$Dataset,occu_RegElev_pred_df$upper, type="l", lty=2, col="blue")
  
#--------------------------------------------------------------------------------
  
#m.psiHFIRoad.pEff 
#---HFI Constant
#create a list of models you are trying to plot
mods <- list(m.psiHFIRoad.pEff = m.psiHFIRoad.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_RoadHFI_newdata <- data.frame(d.Road = seq(min(scale(sc_t$d.Road)), 
                                           max(scale(sc_t$d.Road)), by = 0.1),
                                HFI = mean(scale(sc_t$HFI))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_RoadHFI_pred <- modavgPred(mods,
                             # c.hat =    # to change variance inflation factor, default = 1) 
                             parm.type = "psi", # psi = occupancy
                             newdata = occu_RoadHFI_newdata)[c("mod.avg.pred",
                                                            "lower.CL",
                                                            "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_RoadHFI_pred_df <- data.frame(Predicted = occu_RoadHFI_pred$mod.avg.pred,
                                lower = occu_RoadHFI_pred$lower.CL,
                                upper = occu_RoadHFI_pred$upper.CL,
                                occu_RoadHFI_newdata)


plot(occu_RoadHFI_newdata$d.Road, occu_RoadHFI_pred$mod.avg.pred, type="l", 
     xlab="Distance to Road", ylab="Occupancy probability", main = "Impact of Distance to Road on Baird's Tapir Occupancy (HFI Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_RoadHFI_newdata$d.Road,occu_RoadHFI_pred_df$lower, type="l", lty=2, col="blue")
points(occu_RoadHFI_newdata$d.Road,occu_RoadHFI_pred_df$upper, type="l", lty=2, col="blue")

#---d.Road Constant

#create a list of models you are trying to plot
mods <- list(m.psiHFIRoad.pEff = m.psiHFIRoad.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_HFIRoad_newdata <- data.frame(HFI = seq(min(scale(sc_t$HFI)), 
                                                max(scale(sc_t$HFI)), by = 0.1),
                                   d.Road = mean(scale(sc_t$d.Road))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_HFIRoad_pred <- modavgPred(mods,
                                # c.hat =    # to change variance inflation factor, default = 1) 
                                parm.type = "psi", # psi = occupancy
                                newdata = occu_HFIRoad_newdata)[c("mod.avg.pred",
                                                                  "lower.CL",
                                                                  "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_HFIRoad_pred_df <- data.frame(Predicted = occu_HFIRoad_pred$mod.avg.pred,
                                   lower = occu_HFIRoad_pred$lower.CL,
                                   upper = occu_HFIRoad_pred$upper.CL,
                                   occu_HFIRoad_newdata)


plot(occu_HFIRoad_newdata$HFI, occu_HFIRoad_pred$mod.avg.pred, type="l", 
     xlab="Human Footprint Index (HFI)", ylab="Occupancy probability", main = "Impact of Human Footprint Index (HFI) on Baird's Tapir Occupancy (d.Road Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_HFIRoad_newdata$HFI,occu_HFIRoad_pred_df$lower, type="l", lty=2, col="blue")
points(occu_HFIRoad_newdata$HFI,occu_HFIRoad_pred_df$upper, type="l", lty=2, col="blue")

#--------------------------------------------------------------------------------#
### Three Multicovariates ## ------>>>>TOP MODEL - include in report

#m.psiElevHFIRoad.pEff
#---HFI and d.Road Constant
#create a list of models you are trying to plot
mods <- list(m.psiElevHFIRoad.pEff = m.psiElevHFIRoad.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_ElevHFIRoad_newdata <- data.frame(Elev = seq(min(scale(sc_t$Elev)), 
                                             max(scale(sc_t$Elev)), by = 0.1),
                                   d.Road = mean(scale(sc_t$d.Road)), HFI = mean(scale(sc_t$HFI))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_ElevHFIRoad_pred <- modavgPred(mods,
                                # c.hat =    # to change variance inflation factor, default = 1) 
                                parm.type = "psi", # psi = occupancy
                                newdata = occu_ElevHFIRoad_newdata)[c("mod.avg.pred",
                                                                  "lower.CL",
                                                                  "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_ElevHFIRoad_pred_df <- data.frame(Predicted = occu_ElevHFIRoad_pred$mod.avg.pred,
                                   lower = occu_ElevHFIRoad_pred$lower.CL,
                                   upper = occu_ElevHFIRoad_pred$upper.CL,
                                   occu_ElevHFIRoad_newdata)


plot(occu_ElevHFIRoad_newdata$Elev, occu_ElevHFIRoad_pred$mod.avg.pred, type="l", 
     xlab="Elevation", ylab="Occupancy probability", main = "Impact of Elevation on Baird's Tapir Occupancy (HFI and d.Road Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_ElevHFIRoad_newdata$Elev,occu_ElevHFIRoad_pred_df$lower, type="l", lty=2, col="blue")
points(occu_ElevHFIRoad_newdata$Elev,occu_ElevHFIRoad_pred_df$upper, type="l", lty=2, col="blue")


#---d.road and Elev Constant
#create a list of models you are trying to plot
mods <- list(m.psiElevHFIRoad.pEff = m.psiElevHFIRoad.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_HFIRoadElev_newdata <- data.frame(HFI = seq(min(scale(sc_t$HFI)), 
                                                  max(scale(sc_t$HFI)), by = 0.1),
                                       d.Road = mean(scale(sc_t$d.Road)), Elev = mean(scale(sc_t$Elev))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_HFIRoadElev_pred <- modavgPred(mods,
                                    # c.hat =    # to change variance inflation factor, default = 1) 
                                    parm.type = "psi", # psi = occupancy
                                    newdata = occu_HFIRoadElev_newdata)[c("mod.avg.pred",
                                                                          "lower.CL",
                                                                          "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_HFIRoadElev_pred_df <- data.frame(Predicted = occu_HFIRoadElev_pred$mod.avg.pred,
                                       lower = occu_HFIRoadElev_pred$lower.CL,
                                       upper = occu_HFIRoadElev_pred$upper.CL,
                                       occu_HFIRoadElev_newdata)


plot(occu_HFIRoadElev_newdata$HFI, occu_HFIRoadElev_pred$mod.avg.pred, type="l", 
     xlab="Human Footprint Index (HFI)", ylab="Occupancy probability", main = "Impact of Human Footprint Index (HFI) on Baird's Tapir Occupancy (d.Road and Elevation Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_HFIRoadElev_newdata$HFI,occu_HFIRoadElev_pred_df$lower, type="l", lty=2, col="blue")
points(occu_HFIRoadElev_newdata$HFI,occu_HFIRoadElev_pred_df$upper, type="l", lty=2, col="blue")


#--HFI and Elev Constant
#create a list of models you are trying to plot
mods <- list(m.psiElevHFIRoad.pEff = m.psiElevHFIRoad.pEff)

# First, set-up a new dataframe to predict along a sequence of the covariate.
# Predicting requires all covariates, so let's hold the other covariates constant at their mean value
occu_RoadHFIElev_newdata <- data.frame(d.Road = seq(min(scale(sc_t$d.Road)), 
                                                 max(scale(sc_t$d.Road)), by = 0.1),
                                       HFI = mean(scale(sc_t$HFI)), Elev = mean(scale(sc_t$Elev))) # hold other variables constant


# Model-averaged prediction of occupancy and confidence interval
occu_RoadHFIElev_pred <- modavgPred(mods,
                                    # c.hat =    # to change variance inflation factor, default = 1) 
                                    parm.type = "psi", # psi = occupancy
                                    newdata = occu_RoadHFIElev_newdata)[c("mod.avg.pred",
                                                                          "lower.CL",
                                                                          "upper.CL")]

# Put prediction, confidence interval, and covariate values together in a data frame
occu_RoadHFIElev_pred_df <- data.frame(Predicted = occu_RoadHFIElev_pred$mod.avg.pred,
                                       lower = occu_RoadHFIElev_pred$lower.CL,
                                       upper = occu_RoadHFIElev_pred$upper.CL,
                                       occu_RoadHFIElev_newdata)


plot(occu_RoadHFIElev_newdata$d.Road, occu_RoadHFIElev_pred$mod.avg.pred, type="l", 
     xlab="Distance to Road", ylab="Occupancy probability", main = "Impact of Distance to Road on Baird's Tapir Occupancy (HFI and Elevation Constant)", cex.main = 0.9, ylim=c(0,1))

##add confidence intervals
##lty stands for line type, 2 codes a dashed line
points(occu_RoadHFIElev_newdata$d.Road,occu_RoadHFIElev_pred_df$lower, type="l", lty=2, col="blue")
points(occu_RoadHFIElev_newdata$d.Road,occu_RoadHFIElev_pred_df$upper, type="l", lty=2, col="blue")
  
