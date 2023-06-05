###------------------------------------------------------------------------------###
### Combining datasets from CR
###
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:\\Users\\dgrocha\\OneDrive - Southern Nazarene University\\Desktop\\PLNU\\TapirProject\\CR")
library(unmarked)

tapir_t<- readRDS("tapir_CR.rds")
head(tapir_t)
eff_t<- readRDS("eff_CR.rds")
head(eff_t)
sc_t<- read.csv("cv_t3.csv")
head(sc_t)
sc_t2<- cbind(sc_t[,2:5], round(scale(sc_t[,6:14]),3))

rownames(tapir_t) == rownames(eff_t)
rownames(eff_t) == sc_t$Station

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=sc_t2, obsCovs=list(Eff=eff_t))
summary(umf)

# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis( -0.129)  	# Probability of occupancy
plogis(  -0.832)	# Probability of detection
22/141	#Naive occupancy

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
summary(m.psi1.pEff)

# Running unicovariate models
m.psiElev.pEff<- occu(~Eff~Elev , umf)	# SIGNIFICANT!!
summary(m.psiElev.pEff)
m.psiHFI.pEff<- occu(~Eff~HFI , umf) 	# SIGNIFICANT!!
summary(m.psiHFI.pEff)
m.psiFor.pEff<- occu(~Eff~Forest , umf)
summary(m.psiFor.pEff)
m.psiNPP.pEff<- occu(~Eff~NPP , umf)
summary(m.psiNPP.pEff)
m.psiRoad.pEff<- occu(~Eff~d.Road , umf)	# SIGNIFICANT!!
summary(m.psiRoad.pEff)
m.psiRiver.pEff<- occu(~Eff~d.River , umf)
summary(m.psiRiver.pEff)
m.psiED.pEff<- occu(~Eff~EdgeDens , umf)
summary(m.psiED.pEff)
m.psiPD.pEff<- occu(~Eff~PatchDens , umf)
summary(m.psiPD.pEff)
m.psiDC.pEff<- occu(~Eff~DisjCore , umf)
summary(m.psiDC.pEff)
m.psiReg.pEff<- occu(~Eff~Dataset , umf) ####SOUTH SIGNIFICANT!!######
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
		m.psiReg.pEff)

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
    cex.lab=1.0, main = "d.Road and Baird's Tapir Occupancy",xlab="Dist. to roads (d.Road)", ylab="Probability of Occupancy", 
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

########################################################################################################
### Multicovariate models
# You can see that Road, Region, HFI, Elev performed better than the null model (m.psi1.pEff)
# Next step: construct multivariate models with all possible combinations of Road, Region, HFI and Elev . 
# Check what the best performing model is based on AIC values (the same way you did for the unicovariate models)


##collect in fitList
detList2<-fitList(mod0, m.psi1.pEff,
		
		)

##do AIC model selection
modSel(detList2) 

summary(m.psiRegNPP.pEff)



