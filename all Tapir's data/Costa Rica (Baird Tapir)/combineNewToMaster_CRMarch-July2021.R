#Add new Kamuk 2021-2022 csv to master csv and derive missing column values
#Christian Anderson, 2023-6-15
rm(list= ls())
library(dplyr)
library(lubridate)


setwd("C:/Users/chris/Documents/Research")

raw.new<- read.csv("Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Wild_ID_Kamuk_21_March-July.csv", comment.char = "#")#note: csv has comments
master.old<- read.csv("Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Master(2023-6-15).csv", comment.char="#")

#remove records where family is "" --empty but not NA
raw.new<- filter(raw.new, Family != "")
#lets see what column names need to change
names<- list(names(master.old),names(raw.new) )

#make new df

attach(raw.new)
new_kamuk<- data.frame("Survey.Name" = Project.Name,
                 "Species" = paste(Genus,Species, sep= " "),
                 "Common" = rep(NA, nrow(raw.new)),
                 "Individuals" = Number.of.Animals,
                 "Independent" = rep(NA, nrow(raw.new)),
                 "Site" = rep("Kamuk", nrow(raw.new)),
                 "Trail" = rep("Kamuk", nrow(raw.new)),
                 "Camera.Name" = Camera.Serial.Number,
                 "Paired" = rep(NA, nrow(raw.new)),
                 "Latitude" = Latitude,
                 "Longitude" = Longitude, 
                 "X" = rep(NA, nrow(raw.new)),
                 "Y" = rep(NA, nrow(raw.new)),
                 "Elevation" = rep(NA, nrow(raw.new)), 
                 "Elevation.Num" = rep(NA, nrow(raw.new)), 
                 "Habitat" = "Tropical Montane Cloud Forest",
                 "Season" = rep(NA, nrow(raw.new)),
                 "Camera.Start.Date" = Camera.Start.Date,
                 "Camera.End.Date"= Camera.End.Date,
                 "Date" = Photo.Date,
                 "Time" = Photo.time,
                 "Hour" = rep(NA, nrow(raw.new)),
                 "Minute" = rep(NA, nrow(raw.new)), 
                 "Time.In.Radians" = rep(NA, nrow(raw.new)),
                 "Daytime" = rep(NA, nrow(raw.new)), 
                 "Image.ID" = Raw.Name )
detach(raw.new)

#make date type
new_kamuk$Date<- as.Date(new_kamuk$Date, format = "%Y-%m-%d")

#fill season column, nov-jun is dry in CR
new_kamuk["Season"]<- ifelse(month(new_kamuk$Date) >= 11 | month(new_kamuk$Date) <= 6, "Dry", "Wet")

#get rid of blanks, i.e., ' ' 
Kamuk<- filter(new_kamuk, nchar(Species) > 1)

#read in species list
animals<- read.csv("WildID/5.27.22_species_list.csv")
#remove number column
animals<- animals[,-1]

#combine the common names to the main on the shared species column
Kamuk<- merge(Kamuk, animals, by = "Species", all.x = TRUE)
#rename Common.y to Common
names(Kamuk)[which(names(Kamuk) == "Common.y")]<- "Common"


#clean master to fit Kamuk


#get column names as printed list
# get.columns<- function(df){
#   for(column in list(colnames(df))){
#     print(paste(sQuote(column), collapse = ", "))
#   }
# }

# get.columns(master)
# get.columns(Kamuk)
master<- master.old
#vectorize column names for logical operation
Kamuk.columns<- c(colnames(Kamuk))
master.columns<- c(colnames(master))

#tell me what columns need to be changed. what columns in kamuk are not in master
if (!all(Kamuk.columns %in% master.columns)){
  print(paste(Kamuk.columns[!Kamuk.columns %in% master.columns], "not in data.frame"))
} else{
  print("Correct columns present in data.frame")
}
#remove superfluous column using its index
Kamuk<- Kamuk[,-which(colnames(Kamuk) == "Common.x")]

#save kamuk on its own
write.csv(Kamuk, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Kamuk21_March-July_clean.csv", 
          row.names = FALSE)

joined_df<- rbind(Kamuk, master)#it all comes together


#save combined df
write.csv(joined_df, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Master(2023-6-15).csv",
          row.names= FALSE)

#print(guess_formats(as.Date(joined_df$Date), c("Ymd", "mdY", "ymd", "dmY", "dmy")))
