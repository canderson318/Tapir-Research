#clear system
rm(list=ls())

#set wd
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Malaysia (Malayan Tapir)")


######Read in Tapir table and Effort###########
tapir<- readRDS("Collapsed_Capture_Malayan_Tapir.rds")
eff<- readRDS("Effort_Malayan_Tapir.rds")

######Read in Elev and HFI Table##############
cov<- read.csv("Ma_T_Final_Covs.csv")

library(unmarked)

#-------------------------------------------------------------------------------
# Correlation Test- Spearman's Test! -> VIF in HH package 
cor.test(cov$HFI, cov$d.Road,
         method = "spearm", alternative = "greater",
         conf.level = 0.95, exact = FALSE)  #weak positive correlation
cor.test(cov$HFI, cov$Elev,
         method = "spearm", alternative = "greater",
         conf.level = 0.95, exact = FALSE) #very Weak positive correlation
cor.test(cov$Elev, cov$AvgMinTemp,
         method = "spearm", alternative = "greater",
         conf.level = 0.95, exact = FALSE) #Very Strong negative correlation
cor.test(cov$Elev, cov$AvgMaxTemp,
         method = "spearm", alternative = "greater",
         conf.level = 0.95, exact = FALSE) #Very Strong negative correlation
cor.test(cov$HFI, cov$Forest,
         method = "spearm", alternative = "greater",
         conf.level = 0.95, exact = FALSE) #Very weak positive correlation
cor.test(cov$Forest, cov$NPP,
         method = "spearm", alternative = "greater",
         conf.level = 0.95, exact = FALSE) #Weak positive correlation
#-----------------------------------------------------------------------

#####Model-Prep######################

umf<- unmarkedFrameOccu(y=tapir[,-1], siteCovs= as.data.frame(scale(cov[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf)
head(cov)

######Running Models!####################################
#Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
#summary(mod0)

# Running model with Eff as survey covariate

m.psi1.pEff<- occu(~Eff~1, umf)  # Eff Model
#summary(m.psi1.pEff)

#~1 ~HFI

m.p1.psiHFI<- occu(~1~HFI, umf)
#summary(m.p1.psiHFI)

#~1 ~Elev
m.p1.psiElev<- occu(~1 ~Elev, umf) #--> significant
#cov$Elevation[which(rowSums(puma[,-1], na.rm=T)>0)]
summary(m.p1.psiElev)

#~1 ~Precipitation  
m.p1.psiPrec<- occu(~1~Precip, umf) #--> significant
summary(m.p1.psiPrec)

#~1 ~Road - NEEDS TO BE DONE STILL
m.p1.psiroad<- occu(~1~d.Road, umf) 
summary(m.p1.psiroad)

#~Eff ~HFI
m.pEff.psiHFI<- occu(~Eff ~HFI, umf)
summary(m.pEff.psiHFI)
#hist(cov$HFI)

#~Eff ~Elevation
m.pEff.psiElev<- occu(~Eff ~Elev, umf)
summary(m.pEff.psiElev)

#~Eff ~Precipitaion
m.pEff.psiPrec<- occu(~Eff ~Precip, umf)
summary(m.pEff.psiPrec)
#hist(cov$Precipitation)

#~Eff ~Road
mod.eff.road <- occu(~Eff ~d.Road, umf)
summary(mod.eff.road)
plot(mod.eff.road)

m.psiTempmin.pEff<- occu(~Eff~ AvgMinTemp, umf)
summary(m.psiTempmin.pEff)

m.psiTempmax.pEff<- occu(~Eff~ AvgMaxTemp, umf) 
summary(m.psiTempmax.pEff)

m.psiNDVI.pEff<- occu(~Eff~ NDVI, umf) 
summary(m.psiNDVI.pEff)

##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other
#detList.tapir<-fitList(mod0, m.psi1.pEff, m.p1.psiHFI, m.p1.psiElev, m.p1.psiPrec, m.pEff.psiPrec, m.pEff.psiElev, m.pEff.psiHFI)
detList.tapir<-fitList(mod0, 
                       m.psi1.pEff,
                       m.pEff.psiPrec, 
                       m.pEff.psiElev,
                       mod.eff.road, 
                       m.psiTempmax.pEff,
                       m.psiNDVI.pEff)
# modSel compares AND ranks the models against eachother!
sink("modeseltest.txt")
modSel(detList.tapir)
sink()
#####Plotting##############################################################
hist(cov$d.Road)
#pdf("Tapir_unicovRoad_CRtotal.pdf")
pred.psi.road<- predict(mod.eff.road, newdata= data.frame(d.Road= sort(scale(cov$d.Road))), "state")
plot(sort(cov$d.Road), pred.psi.road$Predicted, type="l", xaxt="n", 
     cex.lab=1.0, main = "Distance to Road and Malayan Tapir Occupancy",xlab="Distance to roads (d.Road)", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, tick = TRUE, )
points(sort(cov$d.Road), pred.psi.road$lower, type="l", lty=2)
points(sort(cov$d.Road), pred.psi.road$upper, type="l", lty=2)

##################Running Multicovariate Models###############################################################################################

# Check what the best performing model is based on AIC values (the same way you did for the unicovariate models)
#Eff, Elev + AvgMaxTemp
#Eff, Elev + Precip
#Eff, Elev + d.Road
#Eff, Elev + NDVI
#Eff, AvgMaxTemp + Precip
#Eff, AvgMaxTemp + d.Road
#Eff, AvgMaxTemp + NDVI
#Eff, Precip + d.Road
#Eff, Precip + NDVI
#Eff, NDVI + d.Road

#Running Null model
mod0 <- occu(~1~1+1, umf)  # Null Model
summary(mod0)
# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1+1, umf)  # Eff Model
summary(m.psi1.pEff)
#Two Variables 
m.psiElevTempmax.pEff<- occu(~Eff~ Elev + AvgMaxTemp, umf)	#Elevation significant!
summary(m.psiElevTempmax.pEff)   
m.psiElevPrecip.pEff<- occu(~Eff~ Elev + Precip, umf)	#Elevation significant!
summary(m.psiElevPrecip.pEff)  
m.psiElevRoad.pEff<- occu(~Eff~ Elev + d.Road, umf)	#Elevation significant!
summary(m.psiElevRoad.pEff) 
m.psiElevNDVI.pEff<- occu(~Eff~ Elev + NDVI, umf)	#Elevation significant!
summary(m.psiElevNDVI.pEff)  
m.psiAvgMaxTempPrecip.pEff<- occu(~Eff~ AvgMaxTemp + Precip, umf)	#Elevation significant!
summary(m.psiAvgMaxTempPrecip.pEff)  
m.psiAvgMaxTempRoad.pEff<- occu(~Eff~ AvgMaxTemp + d.Road, umf)	#Elevation significant!
summary(m.psiAvgMaxTempRoad.pEff)  
m.psiAvgMaxTempNDVI.pEff<- occu(~Eff~ AvgMaxTemp + NDVI, umf)	#Elevation significant!
summary(m.psiAvgMaxTempNDVI.pEff)  
m.psiPrecipRoad.pEff<- occu(~Eff~ Precip + d.Road, umf)	#Elevation significant!
summary(m.psiPrecipRoad.pEff)  
m.psiPrecipNDVI.pEff<- occu(~Eff~ Precip + NDVI, umf)	#Elevation significant!
summary(m.psiPrecipNDVI.pEff)  
m.psiNDVIRoad.pEff<- occu(~Eff~ NDVI + d.Road, umf)	#Elevation significant!
summary(m.psiNDVIRoad.pEff)  

detList.tapir<-fitList(mod0, m.psiElevTempmax.pEff, m.psiElevPrecip.pEff, m.psiElevRoad.pEff, m.psiElevNDVI.pEff, 
                       m.psiAvgMaxTempPrecip.pEff, m.psiAvgMaxTempRoad.pEff, m.psiAvgMaxTempNDVI.pEff, m.psiPrecipRoad.pEff, 
                       m.psiPrecipNDVI.pEff, m.psiNDVIRoad.pEff)
# modSel compares AND ranks the models against each other!
modSel(detList.tapir) ###Only empirically supported two-variable multicovariate models are: ElevRoad and ElevPrecip


###Three variable multicovariate models###
mod0 <- occu(~1~1+1+1, umf)  # Null Model
summary(mod0)
#Eff, Elev + AvgMaxTemp + Precip
m.psiElevTempmaxPrecip.pEff<- occu(~Eff~ Elev + AvgMaxTemp + Precip, umf)	#Elevation significant!
summary(m.psiElevTempmaxPrecip.pEff) 
#Eff, Elev + AvgMaxTemp + d.Road
m.psiElevTempmaxRoad.pEff<- occu(~Eff~ Elev + AvgMaxTemp + d.Road, umf)	#Elevation significant!
summary(m.psiElevTempmaxRoad.pEff) 
#Eff, Elev + AvgMaxTemp + NDVI
m.psiElevTempmaxNDVI.pEff<- occu(~Eff~ Elev + AvgMaxTemp + NDVI, umf)	#Elevation significant!
summary(m.psiElevTempmaxNDVI.pEff) 
#Eff, AvgMaxTime + Precip + d.Road
m.psiTempmaxPrecipRoad.pEff<- occu(~Eff~ AvgMaxTemp + Precip + d.Road, umf)	#Elevation significant!
summary(m.psiTempmaxPrecipRoad.pEff) 
#Eff, AvgMaxTemp + Precip + NDVI
m.psiAvgMaxTempPrecipNDVI.pEff<- occu(~Eff~ AvgMaxTemp + Precip + NDVI, umf)	#Elevation significant!
summary(m.psiAvgMaxTempPrecipNDVI.pEff) 
#Eff, AvgMaxTemp + NDVI + d.Road
m.psiAvgMaxTempNDVIRoad.pEff<- occu(~Eff~ AvgMaxTemp + NDVI + d.Road, umf)	#Elevation significant!
summary(m.psiAvgMaxTempNDVIRoad.pEff) 
#Eff, Precip + d.Road + NDVI
m.psiPrecipRoadNDVI.pEff<- occu(~Eff~ Precip + d.Road + NDVI, umf)	#Elevation significant!
summary(m.psiPrecipRoadNDVI.pEff) 
#Eff, Precip + Elev + NDVI
m.psiPrecipElevNDVI.pEff<- occu(~Eff~ Precip + Elev + NDVI, umf)	#Elevation significant!
summary(m.psiPrecipElevNDVI.pEff) 
#Eff, Precip + Elev + d.Road
m.psiPrecipElevRoad.pEff<- occu(~Eff~ Precip + Elev + d.Road, umf)	#Elevation significant!
summary(m.psiPrecipElevRoad.pEff) 
#Eff, d.Road + NDVI + Elev
m.psiRoadNDVIElev.pEff<- occu(~Eff~ d.Road + NDVI + Elev, umf)	#Elevation significant!
summary(m.psiRoadNDVIElev.pEff) 

detList.tapir<-fitList(mod0, m.psiElevTempmaxPrecip.pEff, m.psiElevTempmaxRoad.pEff, m.psiElevTempmaxNDVI.pEff, m.psiTempmaxPrecipRoad.pEff, 
                       m.psiAvgMaxTempPrecipNDVI.pEff, m.psiAvgMaxTempNDVIRoad.pEff, m.psiPrecipRoadNDVI.pEff, m.psiPrecipElevNDVI.pEff,
                       m.psiPrecipElevRoad.pEff, m.psiRoadNDVIElev.pEff)
# modSel compares AND ranks the models against each other!
modSel(detList.tapir) ###Only empirically supported two-variable multicovariate models are: ElevRoad and ElevPrecip


m.psiTempElevNDVIRoad.pEff <- occu(~Eff~ AvgMaxTemp + Elev + NDVI + d.Road, umf)	#Elevation significant!
summary(m.psiTempElevNDVIRoad.pEff)        
m.psiElevNDVIPrecipRoad.pEff <- occu(~Eff~ Elev + NDVI + Precip + d.Road, umf)	#Elevation significant!
summary(m.psiElevNDVIPrecipRoad.pEff) 
m.psiTempElevRoadPrecip.pEff <- occu(~Eff~ AvgMaxTemp + Elev + d.Road + Precip, umf)	#Elevation significant!
summary(m.psiTempElevRoadPrecip.pEff)
m.psiTempNDVIRoadPrecip.pEff <- occu(~Eff~ AvgMaxTemp + NDVI + d.Road + Precip, umf)	#Elevation significant!
summary(m.psiTempNDVIRoadPrecip.pEff)    	 
m.psiTempElevNDVIPrecip.pEff <- occu(~Eff~ AvgMaxTemp + Elev + NDVI + Precip, umf)	#Elevation significant!
summary(m.psiTempElevNDVIPrecip.pEff)     	 
mod0 <- occu(~1~1+1+1+1, umf)  # Null Model
summary(mod0)

detList.tapir<-fitList(mod0, m.psiTempElevNDVIRoad.pEff, m.psiElevNDVIPrecipRoad.pEff, m.psiTempElevRoadPrecip.pEff, m.psiTempNDVIRoadPrecip.pEff, 
                       m.psiTempElevNDVIPrecip.pEff)
# modSel compares AND ranks the models against each other!
modSel(detList.tapir) ###Only empirically supported two-variable multicovariate models are: ElevRoad and ElevPrecip


#5 variable multicovariate#
mod0 <- occu(~1~1+1+1+1+1, umf)  # Null Model
summary(mod0)
m.psiTempElevNDVIRoadPrecip.pEff <- occu(~Eff~ AvgMaxTemp + Elev + NDVI + d.Road + Precip, umf)	#Elevation significant!
summary(m.psiTempElevNDVIRoadPrecip.pEff) 
detList.tapir<-fitList(mod0, m.psiTempElevNDVIRoadPrecip.pEff)
# modSel compares AND ranks the models against each other!
modSel(detList.tapir) ###Only empirically supported two-variable multicovariate models are: ElevRoad and ElevPrecip


detList2<-fitList(mod0, m.psi1.pEff, m.pEff.psiPrec, m.pEff.psiElev, mod.eff.road, m.psiTempmax.pEff, m.psiNDVI.pEff,
                  m.psiElevNDVI.pEff, m.psiElevTempmax.pEff, m.psiAvgMaxTempNDVI.pEff, m.psiElevPrecip.pEff,
                  m.psiElevRoad.pEff, m.psiAvgMaxTempPrecip.pEff, m.psiAvgMaxTempRoad.pEff, m.psiPrecipRoad.pEff,
                  m.psiPrecipNDVI.pEff, m.psiNDVIRoad.pEff, m.psiElevTempmaxPrecip.pEff, m.psiElevTempmaxRoad.pEff,
                  m.psiElevTempmaxNDVI.pEff, m.psiTempmaxPrecipRoad.pEff, m.psiAvgMaxTempPrecipNDVI.pEff,
                  m.psiAvgMaxTempNDVIRoad.pEff, m.psiPrecipRoadNDVI.pEff, m.psiPrecipElevNDVI.pEff,
                  m.psiPrecipElevRoad.pEff, m.psiRoadNDVIElev.pEff, m.psiTempElevNDVIRoad.pEff, m.psiElevNDVIPrecipRoad.pEff,
                  m.psiTempElevRoadPrecip.pEff, m.psiTempNDVIRoadPrecip.pEff, m.psiTempElevNDVIPrecip.pEff,
                  m.psiTempElevNDVIRoadPrecip.pEff)


modSel(detList2)

# #print to text file
# sink("malayanModSel.txt")
#   print(modSel(detList2))
# 
# sink()

#(((((((((((((((((((((((((((((((((( -------- Get Psi --------))))))))))))))))))))))))))))))))))

#Function to give occupancy probabilities (psi) for models
pf <- function(x) { 
  
  occu <- 0
  if(length(x@estimates@estimates$state@estimates) > 2) {
    for(i in 2:length(x@estimates@estimates$state@estimates)) {
      occu <- (occu + plogis(x@estimates@estimates$state@estimates[i])) 
    }
    occu <- occu/(length(x@estimates@estimates$state@estimates)-1)
  } else {
    occu <- plogis(x@estimates@estimates$state@estimates[2])
  }
  print(paste("𝜓= ", signif(occu, digits = 4)))
}

# Function to give detection probabilities (p) for models 
pd <- function(x) {
  detp <- 0
  if(length(x@estimates@estimates$det@estimates) > 2) {
    for(i in 2:length(x@estimates@estimates$det@estimates)) {
      detp <- (detp + plogis(x@estimates@estimates$det@estimates[i])) 
    }
    detp <- detp/(length(x@estimates@estimates$det@estimates)-1)
  } else {
    detp <- plogis(x@estimates@estimates$det@estimates[2])
  }
  print(paste("p= ", signif(detp, digits=4)))
}

pfpd<- function(x){
  #print(x@formula)
  pf(x)
  pd(x)
}


objects<- ls()
# 
# #get model names in list with grep
models<- objects[grepl("^m\\.|^mod", objects)]
# 
# #get p and psi for each model 
x = readline("enter model name: ")
  while (x != ""){
    print(x)
    x<- get(x)
    pfpd(x)
    x = readline("enter model name: ")
}
####################Plotting Multicovariates##########################################################################################################################
# 
# #install.packages("AICcmodavg")
# library(AICcmodavg)
# #---d.Road constant
# #create a list of models you are trying to plot
# mods <- list(m.psiElevRoad.pEff = m.psiElevRoad.pEff)
# 
# # First, set-up a new dataframe to predict along a sequence of the covariate.
# # Predicting requires all covariates, so let's hold the other covariates constant at their mean value
# occu_ElevRoad_newdata <- data.frame(Elev = seq(min(cov$Elev), 
#                                                max(cov$Elev)), by = 0.1,
#                                     d.Road = mean(cov$d.Road)) # hold other variables constant
# 
# 
# # Model-averaged prediction of occupancy and confidence interval
# occu_ElevRoad_pred <- modavgPred(mods,
#                                  parm.type = "psi", psi = occupancy,
#                                  newdata = occu_ElevRoad_newdata)[c("mod.avg.pred",
#                                                                     "lower.CL",
#                                                                     "upper.CL")]
# 
# # Put prediction, confidence interval, and covariate values together in a data frame
# occu_ElevRoad_pred_df <- data.frame(Predicted = occu_ElevRoad_pred$mod.avg.pred,
#                                     lower = occu_ElevRoad_pred$lower.CL,
#                                     upper = occu_ElevRoad_pred$upper.CL,
#                                     occu_ElevRoad_newdata)
# 
# 
# plot(occu_ElevRoad_newdata$Elev, occu_ElevRoad_pred$mod.avg.pred, type="l", 
#      xlab="Elevation", ylab="Occupancy probability", main = "Impact of Elevation on Baird's Tapir Occupancy (d.Road Constant)", cex.main = 0.9, ylim=c(0,1))
# 
# ##add confidence intervals
# ##lty stands for line type, 2 codes a dashed line
# points(occu_ElevRoad_newdata$Elev,occu_ElevRoad_pred_df$lower, type="l", lty=2, col="blue")
# points(occu_ElevRoad_newdata$Elev,occu_ElevRoad_pred_df$upper, type="l", lty=2, col="blue")
# 
# #---Elev Constant
# 
# #create a list of models you are trying to plot
# mods <- list(m.psiElevRoad.pEff = m.psiElevRoad.pEff)
# 
# # First, set-up a new dataframe to predict along a sequence of the covariate.
# # Predicting requires all covariates, so let's hold the other covariates constant at their mean value
# occu_RoadElev_newdata <- data.frame(d.Road = seq(min(cov$d.Road), 
#                                                  max(cov$d.Road)), by = 0.1,
#                                     Elev = mean(cov$Elev)) # hold other variables constant
# 
# 
# # Model-averaged prediction of occupancy and confidence interval
# occu_RoadElev_pred <- modavgPred(mods,
#                                  # c.hat =    # to change variance inflation factor, default = 1) 
#                                  parm.type = "psi", # psi = occupancy
#                                  newdata = occu_RoadElev_newdata)[c("mod.avg.pred",
#                                                                     "lower.CL",
#                                                                     "upper.CL")]
# 
# # Put prediction, confidence interval, and covariate values together in a data frame
# occu_RoadElev_pred_df <- data.frame(Predicted = occu_RoadElev_pred$mod.avg.pred,
#                                     lower = occu_RoadElev_pred$lower.CL,
#                                     upper = occu_RoadElev_pred$upper.CL,
#                                     occu_RoadElev_newdata)
# 
# 
# plot(occu_RoadElev_newdata$d.Road, occu_RoadElev_pred$mod.avg.pred, type="l", 
#      xlab="Distance to Road", ylab="Occupancy probability", main = "Impact of Distance to Road on Baird's Tapir Occupancy (Elevation Constant)", cex.main = 0.9, ylim=c(0,1))
# 
# ##add confidence intervals
# ##lty stands for line type, 2 codes a dashed line
# points(occu_RoadElev_newdata$d.Road,occu_RoadElev_pred_df$lower, type="l", lty=2, col="blue")
# points(occu_RoadElev_newdata$d.Road,occu_RoadElev_pred_df$upper, type="l", lty=2, col="blue")
# 
