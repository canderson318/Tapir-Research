###------------------------------------------------------------------------------###
### Combining datasets from AM
###
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:\\Users\\dgrocha\\OneDrive - Southern Nazarene University\\Desktop\\PLNU\\Combining_AMDatasets")

library(unmarked)

paca_t<- readRDS("paca_AM.rds")
head(paca_t)
eff_t<- readRDS("eff_AM.rds")
head(eff_t)
sc_t<- read.csv("cv_t_AM_v2.csv")
head(sc_t)
sc_t2<- cbind(sc_t[,1:4], round(scale(sc_t[,5:14]),3))

rownames(paca_t) == rownames(eff_t)
rownames(eff_t) == sc_t2$Station

umf<- unmarkedFrameOccu(y=paca_t, siteCovs=sc_t2, obsCovs=list(Eff=eff_t))
summary(umf)

# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis( -0.485)  	# Probability of occupancy
plogis(  -0.673)	# Probability of detection
63/182	#Naive occupancy

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

pdf("Paca_unicovRegion_AMtotal.pdf")
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

pdf("Paca_unicovELEV_AMtotal.pdf")
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

pdf("Paca_unicovNPP_AMtotal.pdf")
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

pdf("Paca_unicovHFI_AMtotal.pdf")
# Plotting top Unicovariate model (HFI)
pred.psi.hfi<- predict(m.psiHFI.pEff, 
	newdata= data.frame(HFI= sort(scale(sc_t2$HFI))), "state")
plot(sort(scale(sc_t2$HFI)), pred.psi.hfi$Predicted, type="l", xaxt="n", 
	cex.lab=1.3,	xlab="Human Footprint Index (HFI)", ylab="Probability of Occupancy", 
	lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$HFI)), labels = round(sort(sc_t$HFI),0))
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$lower, type="l", lty=2)
points(sort(scale(sc_t2$HFI)), pred.psi.hfi$upper, type="l", lty=2)
dev.off()























