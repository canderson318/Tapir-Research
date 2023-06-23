#clear system
rm(list=ls())

#set wd
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Peru (Mountain Tapir)")
dir()

######Read in Tapir table and Effort###########
tapir<- readRDS("Model Selection/Collapsed_Capture_Mountain_Tapir_revised_DR.rds")
eff<- readRDS("Model Selection/Effort_Mountain_Tapir_revised_DR.rds")

######Read in Elev and HFI Table##############
cov<- read.csv("Model Selection/Mt_T_Covs4.csv")

library(unmarked)

#####Model-Prep######################

umf<- unmarkedFrameOccu(y=tapir[,-1], siteCovs= as.data.frame(scale(cov[,-c(1,2,3,4)])), obsCovs=list(Eff=eff[,-1]))
summary(umf)
head(cov)

######Running Models!####################################
#Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf)  # Eff Model
summary(m.psi1.pEff)

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
                       m.pEff.psiPrec, 
                       m.pEff.psiElev,
                       mod.eff.road, 
                       m.psiTempmax.pEff,
                       m.psiNDVI.pEff)
# modSel compares AND ranks the models against each other!
modSel(detList.tapir) #Empirically supported models: Temp Max (TM), Temp Min, Elev, and NDVI.

#####Plotting##############################################################
hist(cov$AvgMaxTemp)
#pdf("Tapir_unicovRoad_CRtotal.pdf")
pred.psi.temp<- predict(m.psiTempmax.pEff, newdata= data.frame(AvgMaxTemp= sort(scale(cov$AvgMaxTemp))), "state")
plot(sort(cov$AvgMaxTemp), pred.psi.temp$Predicted, type="l", xaxt="n", 
     cex.lab=1.0, main = "Average Maximum Temperature and Malayan Tapir Occupancy",xlab="Average Maximum Temperature (C)", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, tick = TRUE, )
points(sort(cov$AvgMaxTemp), pred.psi.temp$lower, type="l", lty=2)
points(sort(cov$AvgMaxTemp), pred.psi.temp$upper, type="l", lty=2)

#Elev
pred.psi.elev<- predict(m.pEff.psiElev, newdata= data.frame(Elev= sort(scale(cov$Elev))), "state")
plot(sort(cov$Elev), pred.psi.elev$Predicted, type="l", xaxt="n", 
     cex.lab=1.0, main = "Elevation and Malayan Tapir Occupancy",xlab="Elevation (m)", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, tick = TRUE, )
points(sort(cov$Elev), pred.psi.elev$lower, type="l", lty=2)
points(sort(cov$Elev), pred.psi.elev$upper, type="l", lty=2)

#NDVI
pred.psi.ndvi<- predict(m.psiNDVI.pEff, newdata= data.frame(NDVI= sort(scale(cov$NDVI))), "state")
plot(sort(cov$NDVI), pred.psi.ndvi$Predicted, type="l", xaxt="n", 
     cex.lab=1.0, main = "NDVI and Malayan Tapir Occupancy",xlab="NDVI", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, tick = TRUE, )
points(sort(cov$NDVI), pred.psi.ndvi$lower, type="l", lty=2)
points(sort(cov$NDVI), pred.psi.ndvi$upper, type="l", lty=2)


##################Running Multicovariate Models###################################

# Check what the best performing model is based on AIC values (the same way you did for the unicovariate models)
#Eff, Elev + NDVI
#Eff, Elev + AvgMaxTemp
#Eff, Elev + AvgMinTemp
#Eff, NDVI + AvgMaxTemp
#Eff, NDVI + AvgMinTemp
#Eff, AvgMaxTemp + AvgMinTemp

#Two Variables 
m.psiElevNDVI.pEff<- occu(~Eff~Elev + NDVI, umf)	
summary(m.psiElevNDVI.pEff)     
m.psiElevMaxTemp.pEff<- occu(~Eff~Elev + AvgMaxTemp, umf)	
summary(m.psiElevMaxTemp.pEff)    
m.psiNDVIMaxTemp.pEff<- occu(~Eff~NDVI + AvgMaxTemp, umf)	
summary(m.psiNDVIMaxTemp.pEff)     
m.psiElevPrecip.pEff<- occu(~Eff~ Elev + Precip, umf)	#Elevation significant!
summary(m.psiElevPrecip.pEff)  
plogis(0.000683) #Occupancy Estimate (Elev) = 0.5001707
plogis( 0.004718) #Occupancy Estimate (Precip) = 0.5011795
(0.5001707+0.5011795)/2  # = 0.5006751!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plogis( -0.0331) #Detection Estimate= 0.4917258
m.psiElevRoad.pEff<- occu(~Eff~ Elev + d.Road, umf)	#Elevation significant!
summary(m.psiElevRoad.pEff) 
m.psiAvgMaxTempPrecip.pEff<- occu(~Eff~ AvgMaxTemp + Precip, umf)	#Elevation significant!
summary(m.psiAvgMaxTempPrecip.pEff)  
plogis(-0.1298 ) #Occupancy Estimate (Temp) = 0.4675955
plogis( 0.0131) #Occupancy Estimate (Precip) = 0.503275
(0.4675955+0.503275)/2  # = 0.4854352!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plogis(-0.050) #Detection Estimate= 0.4875026
m.psiAvgMaxTempRoad.pEff<- occu(~Eff~ AvgMaxTemp + d.Road, umf)	#Elevation significant!
summary(m.psiAvgMaxTempRoad.pEff)  
m.psiPrecipRoad.pEff<- occu(~Eff~ Precip + d.Road, umf)	#Elevation significant!
summary(m.psiPrecipRoad.pEff)  
m.psiPrecipNDVI.pEff<- occu(~Eff~ Precip + NDVI, umf)	#Elevation significant!
summary(m.psiPrecipNDVI.pEff)  
m.psiNDVIRoad.pEff<- occu(~Eff~ NDVI + d.Road, umf)	#Elevation significant!
summary(m.psiNDVIRoad.pEff)

#Three Variables
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
plogis(-0.1201) #Occupancy Estimate (Temp) = 0.470011
plogis(0.0136) #Occupancy Estimate (Precip) = 0.5033999
plogis(-2.0362) #Occupancy Estimate (NDVI) = 0.1154542
(0.470011+0.5033999+0.1154542)/3  # = 0.362955!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plogis(-0.0502) #Detection Estimate= 0.4874526
#Eff, AvgMaxTemp + NDVI + d.Road
m.psiAvgMaxTempNDVIRoad.pEff<- occu(~Eff~ AvgMaxTemp + NDVI + d.Road, umf)	#Elevation significant!
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

#Five Variables

m.psiTempElevNDVIRoadPrecip.pEff <- occu(~Eff~ AvgMaxTemp + Elev + NDVI + d.Road + Precip, umf)	#Elevation significant!
summary(m.psiTempElevNDVIRoadPrecip.pEff)

##collect in fitList ---> 2 variables 
detList2<-fitList(mod0, 
                  m.pEff.psiPrec, 
                  m.pEff.psiElev, 
                  mod.eff.road, 
                  m.psiTempmax.pEff,
                  m.psiNDVI.pEff,
                  m.psiElevNDVI.pEff, 
                  m.psiElevMaxTemp.pEff, 
                  m.psiNDVIMaxTemp.pEff,
                  m.psiElevPrecip.pEff,
                  m.psiElevRoad.pEff, 
                  m.psiAvgMaxTempPrecip.pEff, 
                  m.psiAvgMaxTempRoad.pEff,
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
                  m.psiTempElevNDVIRoadPrecip.pEff)

##do AIC model selection
modSel(detList2) 
#Top Model (for 2 variable) according to AIC= m.psiElevRoad.pEff 
summary(m.psiElevRoad.pEff)




#((((((((((((((((((((((((((((((((((((((((((( >>Finding Psi, AIC<<< )))))))))))))))))))))))))))))))))))))))))))

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
  print(x@formula)
  pf(x)
  pd(x)
}


# sink("mountainModelSelectionOutput.txt")
# 
#   # R code that generates output
#   print(modSel(detList2))
# 
# 
# # Close the file connection
# sink()






