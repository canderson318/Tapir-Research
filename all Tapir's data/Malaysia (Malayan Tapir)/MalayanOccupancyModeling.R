#clear system
rm(list=ls())

#set wd
setwd("~/Desktop/ Honor's Project/Malayan Tapir Data")
dir()

######Read in Tapir table and Effort###########
tapir<- readRDS("Collapsed_Capture_Malayan_Tapir.rds")
eff<- readRDS("Effort_Malayan_Tapir.rds")

######Read in Elev and HFI Table##############
cov<- read.csv("Ma_T_Final_Covs.csv")

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

#~1 ~HFI

m.p1.psiHFI<- occu(~1~HFI, umf)
summary(m.p1.psiHFI)

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

##################What's the best model?###################################

#detList is the name of the list, fitList compares the models with each other
detList.tapir<-fitList(mod0, m.psi1.pEff, m.p1.psiHFI, m.p1.psiElev, m.p1.psiPrec, m.pEff.psiPrec, m.pEff.psiElev, m.pEff.psiHFI)
detList.tapir<-fitList(mod0, m.pEff.psiPrec, m.pEff.psiElev, m.pEff.psiHFI, mod.eff.road)
# modSel compares AND ranks the models against eachother!
modSel(detList.tapir)

Malayan_AIC_Table<-modSel(detList.tapir)
str(Malayan_AIC_Table)
ModelRankMalayan<-as.data.frame(Malayan_AIC_Table@Full)
#write.csv(ModelRankMalayan, "Malayan_AIC_Table.csv")
