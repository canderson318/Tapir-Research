############################################
#This script adds the NPP (net primary producer) field to the Mt_t_Covs4.csv
# CHristian anderson 17jul2023
############################################

rm(list = ls())

setwd("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/")
MT_covs<-read.csv("Model Selection/Mt_T_Covs4.csv")
npp<- read.csv("mtNPP(May25-July12 2016)/MT_NPP_new.csv")

#rename headers to incoming's names
newnames<- c("lat", "lon")
names(MT_covs)[3:4]<- newnames


mt<- merge(MT_covs, npp[,-3], by= c("lat", "lon"), all.x= TRUE )

#change col name
names(mt)[ncol(mt)]<- "NPP"

#write.csv(mt, "MT_covs_new.csv")

df<- data.frame(scaledOldNPP = scale(order(npp$NPP)), scaledNewNPP = scale(order(npp$newNPP1)))
ggplot(data= df, aes(x = scaledOldNPP, y= scaledNewNPP))+
  geom_smooth()

cor(df)
