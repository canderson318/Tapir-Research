###------------------------------------------------------------------------------###
### Combining datasets from AM
### Sarah Turcic (Edited by DR on 10Oct2022
### Date: 9/12/22
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Amazon (Lowland Tapir)")

library(unmarked)

#Read in Variables###################################################################

#read in tapir occurance records
tapir_t<- readRDS("tapir_AM.rds")
head(tapir_t)

#read in effort table
eff_t<- readRDS("eff_AM.rds")
head(eff_t)

#read in covariate table
sc_t<- read.csv("cv_t_AM_v2.csv")
head(sc_t)

#scale covariates
sc_t2<- cbind(sc_t[,c(1:4)], round(scale(sc_t[,5:ncol(sc_t)]),3))

#ensure rownames match
rownames(tapir_t) == rownames(eff_t)
rownames(eff_t) == sc_t2$Station

# Checking for sitecov correlations
head(sc_t2)
as.dist(cor(sc_t2[,-c(1:4)]))
# Some sitecovs are correlated: Road&Elev, ED&PD&DC (do not include correlated covs in the same model)

#Establish Unmarked Data Frame##############################################################

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=sc_t2, obsCovs=list(Eff=eff_t))
summary(umf) #67 sites with detection


#Running Models#######################################################################

# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis( 0.125)  	# Probability of occupancy
plogis( -1.52)	# Probability of detection
67/182	#Naive occupancy - 0.3681319

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
summary(m.psi1.pEff)

# Running unicovariate models
m.psiElev.pEff<- occu(~Eff~Elev , umf)
summary(m.psiElev.pEff)
m.psiHFI.pEff<- occu(~Eff~HFI , umf)
summary(m.psiHFI.pEff)
m.psiFor.pEff<- occu(~Eff~Forest , umf)
summary(m.psiFor.pEff)
m.psiNPP.pEff<- occu(~Eff~NPP , umf)
summary(m.psiNPP.pEff)
m.psiRoad.pEff<- occu(~Eff~d.Road , umf)  
plogis(0.156)
plogis(0.265)
summary(m.psiRoad.pEff)
m.psiRiver.pEff<- occu(~Eff~d.River, umf)   
summary(m.psiRiver.pEff)
m.psiED.pEff<- occu(~Eff~EdgeDens , umf)
summary(m.psiED.pEff)
m.psiPD.pEff<- occu(~Eff~PatchDens , umf)
summary(m.psiPD.pEff)
m.psiDC.pEff<- occu(~Eff~DisjCore , umf)
summary(m.psiDC.pEff)
m.psiWat.pEff<- occu(~Eff~Water , umf)
summary(m.psiWat.pEff)
m.psiReg.pEff<- occu(~Eff~Dataset , umf)
summary(m.psiReg.pEff)
m.psiNDVI.pEff<- occu(~Eff~NDVI , umf)
summary(m.psiNDVI.pEff)
m.psiTemp.pEff<- occu(~Eff~Avg.Max.Temp , umf)
summary(m.psiTemp.pEff)
m.psiPrecip.pEff<- occu(~Eff~MAP , umf)
summary(m.psiPrecip.pEff)


##collect in fitList
detList<-fitList(mod0, m.psi1.pEff,
		m.psiElev.pEff, 
		m.psiFor.pEff,
		m.psiNPP.pEff,
		m.psiRoad.pEff,
		m.psiRiver.pEff,
		m.psiED.pEff,
		m.psiPD.pEff,
		m.psiDC.pEff,
		m.psiWat.pEff,
		m.psiReg.pEff,
		m.psiNDVI.pEff,
		m.psiTemp.pEff,
	  m.psiPrecip.pEff)

##do AIC model selection
modSel(detList) 

summary(m.psiHFI.pEff) #HFI to 0.0678 effort to 0.0165
summary(m.psiRoad.pEff) #Road to 0.055 effort to 0.0162

summary(m.psiPD.pEff)
summary(m.psiED.pEff)
summary(m.psiReg.pEff)
summary(m.psi1.pEff)
summary(m.psiNDVI.pEff)

# Plotting top Unicovariate model (HFI)
#pdf("Tapir_unicovHFI_AMtotal.pdf")
pred.psi.hfi<- predict(m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(sc_t2$HFI))), "state")
plot(sort(scale(sc_t2$HFI)), pred.psi.hfi$Predicted, type="l", xaxt="n", 
    cex.lab=1.0, main = "HFI and Brazillian Tapir Occupancy",xlab="Human Footprint Index (HFI)", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, seq(-1,10,1))
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$lower, type="l", lty=2)
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$upper, type="l", lty=2)
#dev.off()

#pdf("Tapir_unicovHFI_AMtotal_v2.pdf")
pred.psi.hfi<- predict(m.psiHFI.pEff, newdata= data.frame(HFI= sort(scale(sc_t2$HFI))), "state")
plot(sort(scale(sc_t2$HFI)), pred.psi.hfi$Predicted, type="l", xaxt="n", 
    cex.lab=1.0, main = "HFI and Brazillian Tapir Occupancy",xlab="Human Footprint Index (HFI)", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, at = sort(scale(sc_t2$HFI)), labels = round(sort(sc_t$HFI),0))
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$lower, type="l", lty=2)
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$upper, type="l", lty=2)
#dev.off()

# Plotting top Unicovariate model (Road) 
#pdf("Tapir_unicovRoad_AMtotal.pdf")
pred.psi.road<- predict(m.psiRoad.pEff, newdata= data.frame(d.Road= sort(scale(sc_t2$d.Road))), "state")
plot(sort(scale(sc_t2$d.Road)), pred.psi.road$Predicted, type="l", xaxt="n", 
    cex.lab=1.3,	xlab="Distance to Road", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$d.Road)), labels = round(sort(sc_t$d.Road),0))
points(sort(scale(sc_t2$d.Road)), pred.psi.road$lower, type="l", lty=2)
points(sort(scale(sc_t2$d.Road)), pred.psi.road$upper, type="l", lty=2)
#dev.off()

# Plotting top Unicovariate model (PD) -----> graph isn't good
hist(sc_t2$PatchDens)	# There is very low variability in PD, not a good variable for this dataset
#pdf("Tapir_unicovPD_AMtotal.pdf")
pred.psi.pd<- predict(m.psiPD.pEff, newdata= data.frame(PatchDens= sort(scale(sc_t2$PatchDens))), "state")
plot(sort(scale(sc_t2$PatchDens)), pred.psi.pd$Predicted, type="l", xaxt="n", 
     cex.lab=1.3,	xlab="Patch Density", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$PatchDens)), labels = round(sort(sc_t$PatchDens),0))
points(sort(scale(sc_t2$PatchDens)), pred.psi.pd$lower, type="l", lty=2)
points(sort(scale(sc_t2$PatchDens)), pred.psi.pd$upper, type="l", lty=2)
#dev.off()

# Plotting top Unicovariate model (ED) 
hist(sc_t2$EdgeDens) 	# There isnot a ton of variability in ED, but better than PD
#pdf("Tapir_unicovED_AMtotal.pdf")
pred.psi.ed<- predict(m.psiED.pEff, newdata= data.frame(EdgeDens= sort(scale(sc_t2$EdgeDens))), "state")
plot(sort(scale(sc_t2$EdgeDens)), pred.psi.ed$Predicted, type="l", xaxt="n", 
     cex.lab=1.3,	xlab="Edge Density", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$EdgeDens)), labels = round(sort(sc_t$EdgeDens),0))
points(sort(scale(sc_t2$EdgeDens)), pred.psi.ed$lower, type="l", lty=2)
points(sort(scale(sc_t2$EdgeDens)), pred.psi.ed$upper, type="l", lty=2)
#dev.off()

# Plotting top Unicovariate model (Regions)
#pdf("Tapir_unicovRegions_AMtotal.pdf")
pred.psi.reg<- predict(m.psiReg.pEff, newdata= data.frame(Dataset= sort(unique(sc_t2$Dataset))), "state")
plot(1:4, pred.psi.reg$Predicted, xaxt="n", cex.lab=1.3, pch=19, cex=1.3,
	xlab="Regions", ylab="Probability of Occupancy", col="blue", ylim= c(0,1),
	 xlim= c(0.5,4.5))
arrows(1:4, pred.psi.reg$lower, 1:4, pred.psi.reg$upper, code = 3, angle = 90,
	length = 0.05)
axis(side = 1, at = 1:4, labels = sort(unique(sc_t2$Dataset)))
#dev.off()


#Much Lower Models#############################################################

# Plotting top Unicovariate model (Elev)
#pdf("Tapir_unicovElev_AMtotal.pdf")
pred.psi.elev<- predict(m.psiElev.pEff, newdata= data.frame(Elev= sort(scale(sc_t2$Elev))), "state")
plot(sort(scale(sc_t2$Elev)), pred.psi.elev$Predicted, type="l", xaxt="n", 
	cex.lab=1.3,	xlab="Elevation (m)", ylab="Probability of Occupancy", 
	lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$Elev)), labels = round(sort(sc_t$Elev),0))
points(sort(scale(sc_t2$Elev)), pred.psi.elev$lower, type="l", lty=2)
points(sort(scale(sc_t2$Elev)), pred.psi.elev$upper, type="l", lty=2)
#dev.off()

# Plotting top Unicovariate model (NPP)
#pdf("Tapir_unicovNPP_AMtotal.pdf")
pred.psi.npp<- predict(m.psiNPP.pEff, newdata= data.frame(NPP= sort(scale(sc_t2$NPP))), "state")
plot(sort(scale(sc_t2$NPP)), pred.psi.npp$Predicted, type="l", xaxt="n", 
	cex.lab=1.3,	xlab="Net Primary Productivity (NPP)", ylab="Probability of Occupancy", 
	lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$NPP)), labels = round(sort(sc_t$NPP),0))
points(sort(scale(sc_t2$NPP)), pred.psi.npp$lower, type="l", lty=2)
points(sort(scale(sc_t2$NPP)), pred.psi.npp$upper, type="l", lty=2)
#dev.off()

# Plotting top Unicovariate model (NDVI)
pred.psi.NDVI<- predict(m.psiNDVI.pEff, newdata= data.frame(NDVI= sort(scale(sc_t2$NDVI))), "state")
plot(sort(scale(sc_t2$NDVI)), pred.psi.NDVI$Predicted, type="l", xaxt="n", 
     cex.lab=1.0, main = "NDVI and Brazillian Tapir Occupancy",xlab="Normalized Difference Vegetation Index (NDVI)", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, seq(-1,10,1))
points(sort(scale(sc_t2$NDVI)), pred.psi.NDVI$lower, type="l", lty=2)
points(sort(scale(sc_t2$NDVI)), pred.psi.NDVI$upper, type="l", lty=2)
#dev.off()


########################################################################################################
### Multicovariate models
# You can see that HFI, Road, PD, ED and Region performed better than the null model (m.psi1.pEff)
# PD and ED are correlated, so lets use ED because there is more variability in it.
# Next step: construct multivariate models with all possible combinations of HFI, Road, ED and Region. 
# Check what the best performing model is based on AIC values (the same way you did for the unicovariate models)

# 2 covariates
m.psiTempElev.pEff <- occu(~Eff~Avg.Max.Temp + Elev, umf)
m.psiTempNDVI.pEff <- occu(~Eff~NDVI + Avg.Max.Temp, umf)
m.psiTempRoad.pEff <- occu(~Eff~d.Road + Avg.Max.Temp, umf)
m.psiTempPrecip.pEff <- occu(~Eff~MAP + Avg.Max.Temp, umf)
m.psiElevNDVI.pEff <- occu(~Eff~NDVI + Elev, umf)
m.psiElevRoad.pEff <- occu(~Eff~Elev + d.Road, umf)
m.psiElevPrecip.pEff <- occu(~Eff~Elev + MAP, umf)
m.psiNDVIroad.pEff <- occu(~Eff~NDVI + d.Road, umf)
m.psiNDVIprecip.pEff <- occu(~Eff~NDVI + MAP, umf)
m.psiPrecipRoad.pEff <- occu(~Eff~MAP + d.Road, umf)

detList2 <- fitList(mod0,
                    m.psiTempElev.pEff, 
                    m.psiTempNDVI.pEff,
                    m.psiTempRoad.pEff,
                    m.psiTempPrecip.pEff, 
                    m.psiElevNDVI.pEff, 
                    m.psiElevRoad.pEff, 
                    m.psiElevPrecip.pEff, 
                    m.psiNDVIroad.pEff, 
                    m.psiNDVIprecip.pEff, 
                    m.psiPrecipRoad.pEff)
modSel(detList2)

# 3 covariates
m.psiTempElevNDVI.pEff <- occu(~Eff~Avg.Max.Temp + Elev + NDVI, umf) 
m.psiTempElevRoad.pEff <- occu(~Eff~Elev + Avg.Max.Temp + d.Road, umf) 
m.psiTempElevPrecip.pEff <- occu(~Eff~Elev + Avg.Max.Temp + MAP, umf) 
m.psiTempNDVIRoad.pEff <- occu(~Eff~NDVI + Avg.Max.Temp + d.Road, umf) 
m.psiTempNDVIPrecip.pEff <- occu(~Eff~NDVI + MAP + Avg.Max.Temp, umf) 
m.psiTempRoadPrecip.pEff <- occu(~Eff~Avg.Max.Temp + d.Road + MAP, umf) 
m.psiElevRoadNDVI.pEff <- occu(~Eff~Elev + d.Road + NDVI, umf) 
m.psiElevPrecipNDVI.pEff <- occu(~Eff~NDVI + Elev + MAP, umf) 
m.psiElevRoadPrecip.pEff <- occu(~Eff~Elev + d.Road + MAP, umf) 
m.psiNDVIPrecipRoad.pEff <- occu(~Eff~MAP + d.Road + NDVI, umf) 

detList3 <- fitList(mod0,
                    m.psiTempElevNDVI.pEff,
                    m.psiTempElevRoad.pEff,
                    m.psiTempElevPrecip.pEff,
                    m.psiTempNDVIRoad.pEff,
                    m.psiTempNDVIPrecip.pEff,
                    m.psiTempRoadPrecip.pEff,
                    m.psiElevRoadNDVI.pEff,
                    m.psiElevPrecipNDVI.pEff,
                    m.psiElevRoadPrecip.pEff,
                    m.psiNDVIPrecipRoad.pEff)

modSel(detList3)

detListf<-fitList(mod0,
                  m.psi1.pEff,
                 m.psiElev.pEff, 
                 m.psiRoad.pEff,
                 m.psiNDVI.pEff,
                 m.psiTemp.pEff,
                 m.psiPrecip.pEff,
                 m.psiTempElev.pEff, 
                 m.psiTempNDVI.pEff,
                 m.psiTempRoad.pEff,
                 m.psiTempPrecip.pEff, 
                 m.psiElevNDVI.pEff, 
                 m.psiElevRoad.pEff, 
                 m.psiElevPrecip.pEff, 
                 m.psiNDVIroad.pEff, 
                 m.psiNDVIprecip.pEff, 
                 m.psiPrecipRoad.pEff,
                 m.psiTempElevNDVI.pEff,
                 m.psiTempElevRoad.pEff,
                 m.psiTempElevPrecip.pEff,
                 m.psiTempNDVIRoad.pEff,
                 m.psiTempNDVIPrecip.pEff,
                 m.psiTempRoadPrecip.pEff,
                 m.psiElevRoadNDVI.pEff,
                 m.psiElevPrecipNDVI.pEff,
                 m.psiElevRoadPrecip.pEff,
                 m.psiNDVIPrecipRoad.pEff
                 )
modSel(detListf)

# 4 covariates 
m.psiTempElevNDVIRoad.pEff <- occu(~Eff~Avg.Max.Temp + Elev + NDVI + d.Road, umf) 
m.psiTempElevNDVIPrecip.pEff <- occu(~Eff~Elev + Avg.Max.Temp + NDVI + MAP, umf) 
m.psiTempElevRoadPrecip.pEff <- occu(~Eff~Elev + Avg.Max.Temp + MAP + d.Road, umf) 
m.psiTempNDVIRoadPrecip.pEff <- occu(~Eff~NDVI + Avg.Max.Temp + d.Road + MAP, umf) 
m.psiElevNDVIPrecipRoad.pEff <- occu(~Eff~NDVI + MAP + Elev + d.Road, umf) 

detList4 <- fitList(mod0,
                    m.psiTempElevNDVIRoad.pEff,
                    m.psiTempElevNDVIPrecip.pEff,
                    m.psiTempElevRoadPrecip.pEff,
                    m.psiTempNDVIRoadPrecip.pEff,
                    m.psiElevNDVIPrecipRoad.pEff)

modSel(detList4)

# 5 covariates 
m.psiTempElevNDVIRoadPrecip.pEff <- occu(~Eff~Avg.Max.Temp + Elev + NDVI + d.Road + MAP, umf) 

detListfinal <- fitList(mod0, 
                        m.psi1.pEff,
                        m.psiElev.pEff, 
                        m.psiFor.pEff,
                        m.psiRoad.pEff,
                        m.psiNDVI.pEff,
                        m.psiTemp.pEff,
                        m.psiPrecip.pEff,
                        m.psiTempElev.pEff, 
                        m.psiTempNDVI.pEff,
                        m.psiTempRoad.pEff,
                        m.psiTempPrecip.pEff, 
                        m.psiElevNDVI.pEff, 
                        m.psiElevRoad.pEff, 
                        m.psiElevPrecip.pEff, 
                        m.psiNDVIroad.pEff, 
                        m.psiNDVIprecip.pEff, 
                        m.psiPrecipRoad.pEff,
                        m.psiTempElevNDVI.pEff,
                        m.psiTempElevRoad.pEff,
                        m.psiTempElevPrecip.pEff,
                        m.psiTempNDVIRoad.pEff,
                        m.psiTempNDVIPrecip.pEff,
                        m.psiTempRoadPrecip.pEff,
                        m.psiElevRoadNDVI.pEff,
                        m.psiElevPrecipNDVI.pEff,
                        m.psiElevRoadPrecip.pEff,
                        m.psiNDVIPrecipRoad.pEff,
                        m.psiTempElevNDVIRoadPrecip.pEff,
                        m.psiTempElevNDVIRoad.pEff,
                        m.psiTempElevNDVIPrecip.pEff,
                        m.psiTempElevRoadPrecip.pEff,
                        m.psiTempNDVIRoadPrecip.pEff,
                        m.psiElevNDVIPrecipRoad.pEff)
# sink("lowlandModSel.txt")
# 
# modSel(detListfinal)
# 
# sink()
getModelPsiP<- function(){
  objects<- ls()
  models<- objects[grepl("^m\\.|^mod", objects)]
  
  ##get p and psi for each model#####
  
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
  
  #perform function input model for object name
  printPsiP<- function(){
    n= TRUE
    x= ' '
    while (x != ""){
      if (n){
        x = readline("enter model name: ")
        n= FALSE
      }
      if (x !=''){
        print(x)
        x<- get(x)
        pfpd(x)
        x = readline("enter model name: ")
      }else{break}
    }
  
  }
}






