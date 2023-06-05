###------------------------------------------------------------------------------###
### Malayan Tapir Analysis
### Sarah Turcic
### Date: 3/30/22
###------------------------------------------------------------------------------###

#clear system
rm(list=ls())

#set wd
setwd("C:/Sarah/Point_Loma/Courses/7. Fall 2022 (complete)/Honor's Project/Tapirs!/Malayan Data/Data Processing")
dir()

#load library
library(unmarked)

#Read in Variables###################################################################

#read in tapir occurance records
tapir_t<- readRDS("Collapsed_Capture_Malayan_Tapir.rds") 
head(tapir_t)

#read in effort table
eff_t<- readRDS("Effort_Malayan_Tapir.rds")
head(eff_t)

#read in covariate table
sc_t<- read.csv("Ma_T_Final_Covs.csv")
head(sc_t)

#edit covariate table
#colnames(sc_t)[colnames(sc_t) == "Elevation"] ="Elev"
#colnames(sc_t)[colnames(sc_t) == "Precipitation"] ="Precip"

#scale covariates and rename to match previous code
sc_t2<- cbind(sc_t[,2:4], round(scale(sc_t[,c(5:8)]),3))

#ensure rownames match
rownames(tapir_t) == rownames(eff_t)

#effort
sum(rowSums(eff_t, na.rm = TRUE)) #28,792 effort
mean(rowSums(eff_t, na.rm = TRUE))	#87.513 mean camera active days
sd(rowSums(eff_t, na.rm = TRUE)) 	#sd mean camera active days 27.73
sum(rowSums(tapir_t, na.rm = TRUE)) #339 independent records


#Establish Unmarked Data Frame##############################################################

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=sc_t2, obsCovs=list(Eff=eff_t))
summary(umf) #150 sites with at least one detection!


#Running Models#######################################################################

# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis( 0.188)  	# Probability of occupancy 0.5468
plogis( -1.82)	# Probability of detection 0.1394
150/329 #Naive occupancy - 0.4559

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
summary(m.psi1.pEff)

# Running unicovariate models
#Elev
m.psiElev.pEff<- occu(~Eff~Elev , umf)
summary(m.psiElev.pEff)
plogis( 1.019) # Probability of occupancy 0.734
plogis(0.342)	# Probability of detection 0.5846

#HFI
m.psiHFI.pEff<- occu(~Eff~HFI , umf) 
summary(m.psiHFI.pEff)
plogis(-0.0563) # Probability of occupancy 0.485928
plogis(0.339)	# Probability of detection 0.5839

#d.Road
m.psiRoad.pEff<- occu(~Eff~d.Road , umf)
summary(m.psiRoad.pEff)
plogis(0.421) # Probability of occupancy 0.604
plogis(0.339)	# Probability of detection 0.584

#Precip
m.psiPrecip.pEff <- occu(~Eff~Precip, umf)
summary(m.psiPrecip.pEff)
plogis(0.790) # Probability of occupancy 0.6878
plogis(0.341)	# Probability of detection 0.584433


#select models using in paper
honlist <- fitList(mod0, 
                   m.psiElev.pEff, 
                   m.psiHFI.pEff,
                   m.psiRoad.pEff,
                   m.psiPrecip.pEff)

##do AIC model selection
modSel(honlist) 
#pdf("Tapir_unicovRegion_Mt_total.pdf")


###############################################################

# Plotting top Unicovariate model (Road) 
pred.psi.road<- predict(m.psiRoad.pEff, 
    newdata= data.frame(d.Road= sort(scale(sc_t2$d.Road))), "state")
plot(sort(scale(sc_t2$d.Road)), pred.psi.road$Predicted, type="l", xaxt="n", 
    cex.lab=1.3,	xlab="Distance to Road", ylab="Probability of Occupancy", 
    lwd=2, col="blue", ylim= c(0,1))
axis(side = 1, at = sort(scale(sc_t2$d.Road)), labels = round(sort(sc_t$d.Road),0))
points(sort(scale(sc_t2$d.Road)), pred.psi.road$lower, type="l", lty=2)
points(sort(scale(sc_t2$d.Road)), pred.psi.road$upper, type="l", lty=2)
dev.off()
#pdf("Tapir_unicovRoad_Mt_total.pdf")


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
#pdf("Tapir_unicovElev_Mt_total.pdf")



























