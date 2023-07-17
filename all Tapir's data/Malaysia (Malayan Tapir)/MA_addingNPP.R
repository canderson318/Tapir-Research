############################################
#This script adds the NPP (net primary producer) field to the Mt_t_Covs4.csv
# CHristian anderson 17jul2023
############################################

rm(list = ls())
require(dplyr)

setwd("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Malaysia (Malayan Tapir)/")
MA_covs<-read.csv("Ma_T_Final_Covs.csv")
#quickly rename x and y to Longitude and latitude
new<- c("long", "lat")

#rename x and y to new
colnames(MA_covs)[colnames(MA_covs) %in% c("X", "Y")] <- new

# #Extracting the unique coords #not necessary b/c only one rec for each cam
# unique_coords <- MA_covs %>%
#   distinct(lat, long, .keep_all = FALSE)
# write.csv(unique_coords, "MA_NPP.csv")

#read in new col
npp<- read.csv("MA_NPP.csv")

#rename headers to destination names
#names(npp)<- c("lat", "long", "NPP")

ma<- merge(MA_covs, npp, by= c("lat", "long"), all.x= TRUE )

write.csv(ma, "MA_covs.csv")
View(ma)

