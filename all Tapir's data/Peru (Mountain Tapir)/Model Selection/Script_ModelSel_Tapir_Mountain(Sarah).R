###------------------------------------------------------------------------------###
### Mountain Tapir Analysis
### Sarah Turcic
### Date: 3/12/22
###------------------------------------------------------------------------------###


rm(list=ls())
setwd("C:/Sarah/Point_Loma/Courses/7. Fall 2022 (complete)/Honor's Project/Tapirs!/Peru Data - Mountain Tapir/Raw Data")

library(unmarked)

#Read in Variables###################################################################

#read in tapir occurance records
tapir_t<- readRDS("Collapsed_Capture_Mountain_Tapir_revised_DR.rds") 
head(tapir_t)

#read in effort table
eff_t<- readRDS("Effort_Mountain_Tapir_revised_DR.rds")
head(eff_t)

#read in covariate table
sc_t<- read.csv("Mt_T_Covs4.csv") #only HFI, elev, d.Road (not all covs)
head(sc_t)

#edit covariate table
#sc_t <- sc_t[,c("Camr_Nm","Latitud","Longitd","S06W080.hgt","HFI_MountainTapir_EPSG4326")]
#colnames(sc_t)[colnames(sc_t) == "S06W080.hgt"] ="Elev"
#colnames(sc_t)[colnames(sc_t) == "HFI_MountainTapir_EPSG4326"] ="HFI"
sc_t <- sc_t[,-1] #get rid of random X column

#scale covariates and rename to match previous code
sc_t2<- cbind(sc_t[,1:3], round(scale(sc_t[,c(4,6,7)]),3)) #don't scale HFI because all the same
sc_t2 <- cbind(sc_t2, sc_t[5]) #add HFI back
#colnames(sc_t2)[4]="Elev"
colnames(sc_t2)[1]="Station"

#ensure rownames match
rownames(tapir_t) == rownames(eff_t)
sort(rownames(eff_t)) == sort(sc_t2$Station) #they are the same but it still says false?

#effort
sum(rowSums(eff_t, na.rm = TRUE)) #9,456 effort
mean(rowSums(eff_t, na.rm = TRUE))	#111.25 mean camera active days
sd(rowSums(eff_t, na.rm = TRUE)) 	#sd mean camera active days 30.92
sum(rowSums(tapir_t, na.rm = TRUE)) #85 independent records


#Establish Unmarked Data Frame##############################################################

umf<- unmarkedFrameOccu(y=tapir_t, siteCovs=sc_t2, obsCovs=list(Eff=eff_t))
summary(umf) #28 sites with at least one detection!


#Running Models#######################################################################

# Running Null model
mod0 <- occu(~1~1, umf)  # Null Model
summary(mod0)
plogis( -0.595)  	# Probability of occupancy 0.35
plogis( -1.56)	# Probability of detection 0.17
28/85 #Naive occupancy - 0.3294

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf) 
summary(m.psi1.pEff)

# Running unicovariate models
#Elev
m.psiElev.pEff<- occu(~Eff~Elev , umf)
summary(m.psiElev.pEff)
plogis( 0.299) # Probability of occupancy 0.574
plogis(0.405)	# Probability of detection 0.599

#HFI
m.psiHFI.pEff<- occu(~Eff~HFI , umf) #all have same HFI, so no use for this species
summary(m.psiHFI.pEff)
plogis(-0.1377) # Probability of occupancy 0.46
plogis(0.402)	# Probability of detection 0.60

#d.Road
m.psiRoad.pEff<- occu(~Eff~d.Road , umf)
summary(m.psiRoad.pEff)
plogis( -0.0636) # Probability of occupancy 0.48
plogis(0.402)	# Probability of detection 0.60

#Precip
m.psiPrecip.pEff <- occu(~Eff~Precip, umf)
summary(m.psiPrecip.pEff)
plogis(0.0261) # Probability of occupancy 0.51
plogis(0.402)	# Probability of detection 0.60


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



























