#Add add common names to master_CR data
#Christian Anderson, 2023-6-15
rm(list= ls())
library(dplyr)
library(lubridate)


setwd("C:/Users/chris/Documents/Research")

raw<- read.csv("Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/CR_Master(2023-6-15).csv", comment.char = "#")#note: csv has comments

animals<- read.csv("WildID/2023-5-15_species_list.csv")
# #read in species list
# animals<- read.csv("WildID/5.27.22_species_list.csv")
# #remove number column
# animals<- animals[,-1]
# #remove duplicate oncilla
# animals<- animals[-22,]
# #rename to simple form without subspecies
# animals[29, 1]<- "Leopardus tigrinus"
# 
# #species without common name
# naCommon_species<- unique(raw$Species[is.na(raw$Common)])
# 
#add new rows
new_Sp<- c("Mazama americana", "Mephitis mephitis", "Panthera leo", "Sphiggurus mexicanus", "Leopardus tigrinus oncilla")
new_Com<- c("Red Brocket Deer", "Striped Skunk",      "Lion",       "Mexican hairy dwarf porcupine", "Oncilla")

# animals<- rbind(animals, data.frame(Species = new_Sp, Common = new_Com))
# #save changes to species list file
# write.csv(animals, "WildID/2023-5-15_species_list.csv", row.names = FALSE )
raw["ID"]<- 1:nrow(raw)
#merge 
raw$Fill<-new_Com[match(raw$Species, new_Sp)]

# values_only_in_raw <- anti_join(raw, merged, by = "Image.ID")
# values_only_in_animals <- anti_join(merged, raw, by = "Image.ID")

# merged<- merge(raw, animals, by = "Species", all.x = FALSE, all.y = FALSE)
# names(merged)[which(names(merged) == "Common.y")]<- "Common" #change new column name
# merged<- merged[,-which(colnames(merged) == "Common.x")]



# count(merged[!is.na(merged$Common.x) & !is.na(merged$Common.y),])#n = 359405 both not na
# 
# count(merged[is.na(merged$Common.x) & !is.na(merged$Common.y),])# n = 549 x= na y not
# xNa_ynot<- merged[is.na(merged$Common.x) & !is.na(merged$Common.y),]
# 
# count(merged[!is.na(merged$Common.x) & is.na(merged$Common.y),])# n = 982 x not y= na
# yNa_xnot<- merged[!is.na(merged$Common.x) & is.na(merged$Common.y),]
# 
# count(merged[is.na(merged$Common.x) & is.na(merged$Common.y),]) #n = 0 both na
merged<- raw
merged['Common'] <- coalesce(merged$Common, merged$Fill)

write.csv(merged, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Master_CR(2023-6-16).csv")

#try to select all columns except common.x and comomn.y
#final_df <- merged %>%
  #select(-Common.x, -Common.y)
