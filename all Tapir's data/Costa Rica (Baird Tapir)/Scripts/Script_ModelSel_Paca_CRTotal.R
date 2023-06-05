###------------------------------------------------------------------------------###
### Combining datasets from CR
###
###------------------------------------------------------------------------------###

rm(list=ls())
setwd("C:\\Users\\dgrocha\\OneDrive - Southern Nazarene University\\Desktop\\PLNU\\Combining_CRDatasets")

library(unmarked)

paca_t<- readRDS("paca_CR.rds")
head(paca_t)
eff_t<- readRDS("eff_CR.rds")
head(eff_t)
sc_t<- read.csv("cv_t3.csv")
head(sc_t)

rownames(paca_t) == rownames(eff_t)
rownames(eff_t) == sc_t$Station

umf<- unmarkedFrameOccu(y=paca_t, siteCovs=sc_t, obsCovs=list(Eff=eff_t))
summary(umf)

# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis( -0.719)  	# Probability of occupancy
plogis(  -0.814)	# Probability of detection
44/141	#Naive occupancy

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
summary(m.psiRoad.pEff)
m.psiRiver.pEff<- occu(~Eff~d.River , umf)
summary(m.psiRiver.pEff)
m.psiED.pEff<- occu(~Eff~EdgeDens , umf)
summary(m.psiED.pEff)
m.psiPD.pEff<- occu(~Eff~PatchDens , umf)
summary(m.psiPD.pEff)
m.psiDC.pEff<- occu(~Eff~DisjCore , umf)
summary(m.psiDC.pEff)
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
		m.psiReg.pEff)

##do AIC model selection
modSel(detList) 

# Running multicuvariate model
m.psiRegNPP.pEff<- occu(~Eff~Dataset + NPP , umf)
summary(m.psiRegNPP.pEff)

##collect in fitList
detList2<-fitList(mod0, m.psi1.pEff,
		m.psiNPP.pEff,
		m.psiReg.pEff,
		m.psiRegNPP.pEff)

##do AIC model selection
modSel(detList2) 

summary(m.psiRegNPP.pEff)



