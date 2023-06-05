###------------------------------------------------------------------------------###
### Combining datasets from AM
### Sarah Turcic
### Date: 9/12/22
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:/Work/Point_Loma/Courses/7. Fall 2022/Honor's Project/Amazon Data - Brazillian Tapir")

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
sc_t2<- cbind(sc_t[,1:4], round(scale(sc_t[,5:14]),3))

#ensure rownames match
rownames(tapir_t) == rownames(eff_t)
rownames(eff_t) == sc_t2$Station


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
m.psiRoad.pEff<- occu(~Eff~scale(sc_t2$d.Road) , umf)   	#Scaled
summary(m.psiRoad.pEff)
m.psiRiver.pEff<- occu(~Eff~scale(sc_t2$d.River), umf)   	#Scaled
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

##collect in fitList
detList<-fitList(mod0, m.psi1.pEff,
		m.psiElev.pEff, 
		m.psiHFI.pEff,
		m.psiFor.pEff,
		m.psiNPP.pEff,
		m.psiRoad.pEff,
		m.psiRiver.pEff,
		m.psiED.pEff,
		m.psiPD.pEff,
		m.psiDC.pEff,
		m.psiWat.pEff,
		m.psiReg.pEff)

##do AIC model selection
modSel(detList) 
pdf("Tapir_unicovRegion_AMtotal.pdf")

summary(m.psiHFI.pEff) #HFI to 0.0678 effort to 0.0165
summary(m.psiRoad.pEff) #Road to 0.055 effort to 0.0162

summary(m.psiPD.pEff)
summary(m.psiED.pEff)
summary(m.psiReg.pEff)
summary(m.psi1.pEff)

# Plotting top Unicovariate model (HFI)
pred.psi.hfi<- predict(m.psiHFI.pEff, 
    newdata= data.frame(HFI= sort(scale(sc_t2$HFI))), "state")
plot(sort(scale(sc_t2$HFI)), pred.psi.hfi$Predicted, type="l", xaxt="n", 
    cex.lab=1.0, main = "HFI and Brazillian Tapir Occupancy",xlab="Human Footprint Index (HFI)", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0.15,1))
axis(side = 1, seq(-1,10,1))
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$lower, type="l", lty=2)
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$upper, type="l", lty=2)
dev.off()

# Plotting top Unicovariate model (HFI)
pred.psi.hfi<- predict(m.psiHFI.pEff, 
    newdata= data.frame(HFI= sort(scale(sc_t2$HFI))), "state")
plot(sort(scale(sc_t2$HFI)), pred.psi.hfi$Predicted, type="l", xaxt="n", 
    cex.lab=1.0, main = "HFI and Brazillian Tapir Occupancy",xlab="Human Footprint Index (HFI)", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0.15,1))
#confused by this command - axis(side = 1, at = sort(scale(sc_t2$HFI)), labels = round(sort(sc_t$HFI),0))
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$lower, type="l", lty=2)
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$upper, type="l", lty=2)
dev.off()

pdf("Tapir_unicovHFI_AMtotal.pdf")

# Plotting top Unicovariate model (Road) ----> graph is not good
pred.psi.road<- predict(m.psiRoad.pEff, 
    newdata= data.frame(Road= sort(scale(sc_t2$d.Road))), "state")
plot(sort(scale(sc_t2$d.Road)), pred.psi.road$Predicted, type="l", xaxt="n", 
    cex.lab=1.3,	xlab="Distance to Road", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$d.Road)), labels = round(sort(sc_t$d.Road),0))
points(sort(scale(sc_t2$d.Road)), pred.psi.road$lower, type="l", lty=2)
points(sort(scale(sc_t2$d.Road)), pred.psi.road$upper, type="l", lty=2)
dev.off()

pdf("Tapir_unicovRoad_AMtotal.pdf")

# Plotting top Unicovariate model (PD) -----> graph isn't good
pred.psi.pd<- predict(m.psiPD.pEff, 
                        newdata= data.frame(PatchDens= sort(scale(sc_t2$PatchDens))), "state")
plot(sort(scale(sc_t2$PatchDens)), pred.psi.pd$Predicted, type="l", xaxt="n", 
     cex.lab=1.3,	xlab="Patch Density", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$PatchDens)), labels = round(sort(sc_t$PatchDens),0))
points(sort(scale(sc_t2$PatchDens)), pred.psi.pd$lower, type="l", lty=2)
points(sort(scale(sc_t2$PatchDens)), pred.psi.pd$upper, type="l", lty=2)
dev.off()

pdf("Tapir_unicovPD_AMtotal.pdf")

# Plotting top Unicovariate model (ED) -----> graph isn't good
pred.psi.ed<- predict(m.psiED.pEff, 
                      newdata= data.frame(EdgeDens= sort(scale(sc_t2$EdgeDens))), "state")
plot(sort(scale(sc_t2$EdgeDens)), pred.psi.ed$Predicted, type="l", xaxt="n", 
     cex.lab=1.3,	xlab="Edge Density", ylab="Probability of Occupancy", 
     lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$EdgeDens)), labels = round(sort(sc_t$EdgeDens),0))
points(sort(scale(sc_t2$EdgeDens)), pred.psi.pd$lower, type="l", lty=2)
points(sort(scale(sc_t2$EdgeDens)), pred.psi.pd$upper, type="l", lty=2)
dev.off()

pdf("Tapir_unicovED_AMtotal.pdf")

# Plotting top Unicovariate model (Regions)
pred.psi.reg<- predict(m.psiReg.pEff, 
		newdata= data.frame(Dataset= sort(unique(sc_t2$Dataset))), "state")
plot(1:4, pred.psi.reg$Predicted, xaxt="n", cex.lab=1.3, pch=19, cex=1.3,
	xlab="Regions", ylab="Probability of Occupancy", col="blue", ylim= c(0,1),
	 xlim= c(0.5,4.5))
arrows(1:4, pred.psi.reg$lower, 1:4, pred.psi.reg$upper, code = 3, angle = 90,
	length = 0.05)
axis(side = 1, at = 1:4, labels = sort(unique(sc_t2$Dataset)))
dev.off()

pdf("Tapir_unicovRegions_AMtotal.pdf")


#Much Lower Models#############################################################

# Plotting top Unicovariate model (Elev)
pred.psi.elev<- predict(m.psiElev.pEff , 
	newdata= data.frame(Elev= sort(scale(sc_t2$Elev))), "state")
plot(sort(scale(sc_t2$Elev)), pred.psi.elev$Predicted, type="l", xaxt="n", 
	cex.lab=1.3,	xlab="Elevation (m)", ylab="Probability of Occupancy", 
	lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$Elev)), labels = round(sort(sc_t$Elev),0))
points(sort(scale(sc_t2$Elev)), pred.psi.elev$lower, type="l", lty=2)
points(sort(scale(sc_t2$Elev)), pred.psi.elev$upper, type="l", lty=2)
dev.off()

pdf("Tapir_unicovElev_AMtotal.pdf")

# Plotting top Unicovariate model (NPP)
pred.psi.npp<- predict(m.psiNPP.pEff, 
	newdata= data.frame(NPP= sort(scale(sc_t2$NPP))), "state")
plot(sort(scale(sc_t2$NPP)), pred.psi.npp$Predicted, type="l", xaxt="n", 
	cex.lab=1.3,	xlab="Net Primary Productivity (NPP)", ylab="Probability of Occupancy", 
	lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$NPP)), labels = round(sort(sc_t$NPP),0))
points(sort(scale(sc_t2$NPP)), pred.psi.npp$lower, type="l", lty=2)
points(sort(scale(sc_t2$NPP)), pred.psi.npp$upper, type="l", lty=2)
dev.off()

pdf("Tapir_unicovNPP_AMtotal.pdf")

























