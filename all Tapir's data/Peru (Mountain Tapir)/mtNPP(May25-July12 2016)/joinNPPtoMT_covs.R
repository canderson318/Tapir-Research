############################################
#This script adds the NPP (net primary producer) field to the Mt_t_Covs4.csv
# CHristian anderson 17jul2023
############################################

rm(list = ls())

setwd("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Peru (Mountain Tapir)/")
MT_covs<-read.csv("Model Selection/Mt_T_Covs4.csv")
npp<- read.csv("MT_NPP.csv")
#rename headers to destination names
names(npp)<- c("Latitud", "Longitd", "NPP")

mt<- merge(MT_covs, npp, by= c("Latitud", "Longitd"), all.x= TRUE )

write.csv(mt, "MT_covs.csv")
View(mt)

