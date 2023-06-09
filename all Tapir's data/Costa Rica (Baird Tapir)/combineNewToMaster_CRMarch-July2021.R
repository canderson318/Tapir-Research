#Add new Kamuk 2021-2022 march - july csv to master csv and derive missing column values
#Christian Anderson, 2023-6-15, edited: 2023-7-6 (CC combineNewToMaster_CR.R)
rm(list= ls())
library(dplyr)
library(lubridate)


setwd("C:/Users/chris/Documents/Research")

new.raw<- read.csv("Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Wild_ID_Kamuk_21_March-July.csv",
                   comment.char = "#")#note: csv has comments
master<- read.csv("Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Master(2023-7-7)-1.csv", comment.char="#")


#remove records where family is "" --empty but not NA, and species filled
new.raw<- filter(new.raw, Family != "") #need dplyr or lubridate, can't remember which
new.raw<- filter(new.raw, nchar(Species) > 1)

##lets see what column names need to change
#names<- list(names(master),names(new.raw) )

#make new df with matching columns

attach(new.raw)
new<- data.frame("X.1" = rep(NA, nrow(new.raw)),
                 "Survey.Name" = Project.Name,
                 "Species" = paste(Genus,Species, sep= " "),
                 "Common" = rep(NA, nrow(new.raw)),
                 "Individuals" = Number.of.Animals,
                 "Independent" = rep(NA, nrow(new.raw)),
                 "Site" = rep("Kamuk", nrow(new.raw)),
                 "Trail" = rep("Kamuk", nrow(new.raw)),
                 "Camera.Name" = Camera.Serial.Number,
                 "Paired" = rep(NA, nrow(new.raw)),
                 "Latitude" = Latitude,
                 "Longitude" = Longitude, 
                 "X" = rep(NA, nrow(new.raw)),
                 "Y" = rep(NA, nrow(new.raw)),
                 "Elevation" = rep(NA, nrow(new.raw)), 
                 "Elevation.Num" = rep(NA, nrow(new.raw)), 
                 "Habitat" = "Tropical Montane Cloud Forest",
                 "Season" = rep(NA, nrow(new.raw)),
                 "Camera.Start.Date" = Camera.Start.Date,
                 "Camera.End.Date"= Camera.End.Date,
                 "Date" = Photo.Date,
                 "Time" = Photo.time,
                 "Hour" = rep(NA, nrow(new.raw)),
                 "Minute" = rep(NA, nrow(new.raw)), 
                 "Time.In.Radians" = rep(NA, nrow(new.raw)),
                 "Daytime" = rep(NA, nrow(new.raw)), 
                 "Image.ID" = Raw.Name)
detach(new.raw)

#------ fix dates and add season
#make date type for its format
new$Date<- as.Date(new$Date, format = "%Y-%m-%d")

#fill season column, nov-jun is dry in CR
new["Season"]<- ifelse(month(new$Date) >= 11 | month(new$Date) <= 6, "Dry", "Wet")

#change date to master's format and back to character 
new$Date<- as.character(format(new$Date, format= "%m/%d/%Y"))

#----------add common names

#read in species list
animals<- read.csv("WildID/Kamuk2021-22_WildID_stuff/2023-7-6_species_list.csv", comment.char= "#")

#combine the common names to the main on the shared species column
new_merged<- merge(new, animals, by = "Species", all.x = TRUE)

if ((nrow(new_merged) - nrow(new)) == 0){ print("successfull merge, nrow equal")
}

#rename Common.y to Common
names(new_merged)[which(names(new_merged) == "Common.y")]<- "Common"

#derive independence
unique(new_merged$Independent)

#explanation of function: 
# this func makes vectors of each column lat, long, and datetime and shifts them left one (excluding first value) and right one index (excluding last value) 
# so that each vector overlaps with the subsequent one
# comparing each subsequent value of x<- c(1,2,3,4,5,6) looks like
#x[-1] == x[-nrow(x)] 
#2,3,4,5,6 compares to 
#1,2,3,4,5
test_independence <- function(data) { 
  
  # Find the duplicate records based on latitude, longitude, and time difference and current independent value
  duplicate_indices <- data$Latitude[-1] == data$Latitude[-nrow(data)] &
    data$Longitude[-1] == data$Longitude[-nrow(data)] &
    abs(difftime(data$datetime[-1], data$datetime[-nrow(data)], units = "hours")) <= 0.5
  # Create a new column 'newIndependent' and set its values based on the duplicate indices
  data$newIndependent <- 1
  data$newIndependent[duplicate_indices] <- 0
  
  # Return the modified data frame
  return(data) #return dataframe with independence column filled
}

mycols<- c(1,3,4,6,7,28,21,22)
head(new_merged[,mycols])



#first need datetime column for comparisons in test_independence func
new_merged$datetime <- as.POSIXct(paste(base::as.Date(new_merged$Date, format = "%m/%d/%Y") , new_merged$Time),
                                  tz = "America/Costa_Rica",
                                  format = "%Y-%m-%d %H:%M:%S")


new_merged_ind<- test_independence(new_merged)

head(new_merged_ind[,c(mycols, ncol(new_merged), ncol(new_merged_ind))])

#remove datetime
new_merged_ind<- new_merged_ind[,-which(colnames(new_merged_ind) == "datetime")]

#change newIndependent to indendent and delete newIndependent
new_merged_ind$Independent<- new_merged_ind$newIndependent
new_merged_ind<- new_merged_ind[,-which(colnames(new_merged_ind) == "newIndependent")]

#convert date back to char in master's date format
if (class(new_merged_ind$Date) == "Date"){ 
  new_merged_ind$Date<- as.character(format(new_merged_ind$Date, format= "%m/%d/%Y"))
}else{print("Date not date class. Check format to match month/day/Year ")}



 #Check what dates exist in df
attach(master)
print("min = "); min(as.numeric(substr(Date, (nchar(Date) - 3),  nchar(Date) )))
print("max = "); max(as.numeric(substr(Date, (nchar(Date) - 3),  nchar(Date) )))

#what surveys have 2010 date
unique(Survey.Name[substr(Date, (nchar(Date) - 3),  nchar(Date) ) == "2010"  ])


View(master[master$Survey.Name == "Chirripo 2016" , which(colnames(master) %in% c("Date", "Survey.Name", "Camera.Start.Date", "Image.ID"))])

detach(master)









#((((>> Merge master and new <<))))

# #vectorize column names for logical operation
new_merged_ind_cols<- c(colnames(new_merged_ind))
master_cols<- c(colnames(master))

#tell me what columns need to be changed. what columns in kamuk are not in master
if (!all(new_merged_cols %in% master_cols)){
  print(paste(new_merged_cols[!new_merged_cols %in% master_cols], "not in destination df "))
} else{
  print("Correct columns present in data.frame")
}

#remove superfluous column using its index
Kamuk<- new_merged_ind[,-which(colnames(new_merged_ind) == "Common.x")]

#save kamuk on its own
#write.csv(Kamuk, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Kamuk21-22_March-July_clean(2023-7-6).csv",row.names = FALSE)

joined_df<- rbind(Kamuk, master)#it all comes together (make sure date formats match)

print(unique(guess_formats(joined_df$Date, c("Ymd", "mdY", "ymd", "dmY", "dmy"))))

#check for no forced date formats which convert 2/2/2014 >> 2/2/20
print(joined_df[grep("/20$", joined_df$Date) ,])

#check for new rows
(nrow(joined_df) - (nrow(master) + nrow(new_merged_ind)))

#save combined df
write.csv(joined_df, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Master(2023-7-7)-2.csv",row.names= FALSE)



# #((((((((((((((((((>> PREVIOUS VERSION <<))))))))))))))))))
# #remove records where family is "" --empty but not NA
# raw.new<- filter(raw.new, Family != "")
# #lets see what column names need to change
# names<- list(names(master.old),names(raw.new) )
# 
# #make new df
# 
# attach(raw.new)
# new_kamuk<- data.frame("Survey.Name" = Project.Name,
#                  "Species" = paste(Genus,Species, sep= " "),
#                  "Common" = rep(NA, nrow(raw.new)),
#                  "Individuals" = Number.of.Animals,
#                  "Independent" = rep(NA, nrow(raw.new)),
#                  "Site" = rep("Kamuk", nrow(raw.new)),
#                  "Trail" = rep("Kamuk", nrow(raw.new)),
#                  "Camera.Name" = Camera.Serial.Number,
#                  "Paired" = rep(NA, nrow(raw.new)),
#                  "Latitude" = Latitude,
#                  "Longitude" = Longitude, 
#                  "X" = rep(NA, nrow(raw.new)),
#                  "Y" = rep(NA, nrow(raw.new)),
#                  "Elevation" = rep(NA, nrow(raw.new)), 
#                  "Elevation.Num" = rep(NA, nrow(raw.new)), 
#                  "Habitat" = "Tropical Montane Cloud Forest",
#                  "Season" = rep(NA, nrow(raw.new)),
#                  "Camera.Start.Date" = Camera.Start.Date,
#                  "Camera.End.Date"= Camera.End.Date,
#                  "Date" = Photo.Date,
#                  "Time" = Photo.time,
#                  "Hour" = rep(NA, nrow(raw.new)),
#                  "Minute" = rep(NA, nrow(raw.new)), 
#                  "Time.In.Radians" = rep(NA, nrow(raw.new)),
#                  "Daytime" = rep(NA, nrow(raw.new)), 
#                  "Image.ID" = Raw.Name )
# detach(raw.new)
# 
# #make date type
# new_kamuk$Date<- as.Date(new_kamuk$Date, format = "%Y-%m-%d")
# 
# #fill season column, nov-jun is dry in CR
# new_kamuk["Season"]<- ifelse(month(new_kamuk$Date) >= 11 | month(new_kamuk$Date) <= 6, "Dry", "Wet")
# 
# #get rid of blanks, i.e., ' ' 
# Kamuk<- filter(new_kamuk, nchar(Species) > 1)
# 
# #read in species list
# animals<- read.csv("WildID/5.27.22_species_list.csv")
# #remove number column
# animals<- animals[,-1]
# 
# #combine the common names to the main on the shared species column
# Kamuk<- merge(Kamuk, animals, by = "Species", all.x = TRUE)
# #rename Common.y to Common
# names(Kamuk)[which(names(Kamuk) == "Common.y")]<- "Common"
# 
# 
# #clean master to fit Kamuk
# 
# 
# #get column names as printed list
# # get.columns<- function(df){
# #   for(column in list(colnames(df))){
# #     print(paste(sQuote(column), collapse = ", "))
# #   }
# # }
# 
# # get.columns(master)
# # get.columns(Kamuk)
# master<- master.old
# #vectorize column names for logical operation
# Kamuk.columns<- c(colnames(Kamuk))
# master.columns<- c(colnames(master))
# 
# #tell me what columns need to be changed. what columns in kamuk are not in master
# if (!all(Kamuk.columns %in% master.columns)){
#   print(paste(Kamuk.columns[!Kamuk.columns %in% master.columns], "not in data.frame"))
# } else{
#   print("Correct columns present in data.frame")
# }
# #remove superfluous column using its index
# Kamuk<- Kamuk[,-which(colnames(Kamuk) == "Common.x")]
# 
# #save kamuk on its own
# write.csv(Kamuk, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Kamuk21_March-July_clean.csv", 
#           row.names = FALSE)
# 
# joined_df<- rbind(Kamuk, master)#it all comes together
# 
# 
# #save combined df
# write.csv(joined_df, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Master(2023-6-15).csv",
#           row.names= FALSE)
# 
# #print(guess_formats(as.Date(joined_df$Date), c("Ymd", "mdY", "ymd", "dmY", "dmy")))
