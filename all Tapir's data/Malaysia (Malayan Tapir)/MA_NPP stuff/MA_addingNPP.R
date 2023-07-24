############################################
#This script adds the NPP (net primary producer) field to the Mt_t_Covs4.csv
# CHristian anderson 17jul2023
############################################

rm(list = ls())
require(dplyr)
require(ggplot2)

setwd("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/")
MA_covs<-read.csv("MA_covs.csv")
#quickly rename x and y to Longitude and latitude
#new<- c("long", "lat")

#rename x and y to new if needed
#colnames(MA_covs)[colnames(MA_covs) %in% c("X", "Y")] <- new

# #Extracting the unique coords #not necessary b/c only one rec for each cam
# unique_coords <- MA_covs %>%
#   distinct(lat, long, .keep_all = FALSE)
# write.csv(unique_coords, "MA_NPP.csv")

#read in new col
npp<- read.csv("MA_NPP stuff/MA_NPP_new.csv")
new<- c("lat", "long", "NPP_new")
colnames(npp)<- new


ma<- merge(MA_covs, npp, by= c("lat", "long"), all.x= TRUE )

ma$NPP<- ma$NPP_new; ma<- ma[,-(ncol(ma))]

#write.csv(ma, "MA_covs.csv")
#write.csv(MA_covs, "MA_covs(orig).csv")

#Check relationship between Old and new npp
df<- data.frame(scaledOldNPP = scale(order(MA_covs$NPP)), scaledNewNPP = scale(order(ma$NPP)))
ggplot(data= df, aes(x = scaledOldNPP, y= scaledNewNPP))+
  geom_smooth()
cor(df)

