#This script finds independence based on location, time, and date by running a vectorized operation on a dataframe
#this improves upon the previous version, CR_cleanForIndependenceAt30min.R because it takes into account common name
#so that two records within 30min, at the same location, being different animals, will both be independent
#Christian Anderson 2023-7-10

rm(list = ls())

require(dplyr)
require(lubridate)
setwd("C:/Users/chris/Documents/Research")
Master<- read.csv("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/Master(2023-7-7)-2.csv")

#unique(grep("Kamuk", master$Survey.Name, value = TRUE))

sites<- c("Kamuk 2021 (March-July)", "Kamuk 2021 (July-February)")

#kamuk<- master[master$Survey.Name %in% sites,]


test_independence <- function(dat) { #*updated to create datetime column and order by date ascending 7.10.2023
  
  #create datetime field for time math
  dat$datetime<- as.POSIXct(paste(base::as.Date(dat$Date, format = "%m/%d/%Y") , dat$Time),
                        tz = "America/Costa_Rica",
                        format = "%Y-%m-%d %H:%M:%S")
  
  #order data by datetime ascending (old-new) so that each record follows the previous chronologically
  dat<- dat[order(dat$datetime),]
  
  # Find the duplicate records based on latitude, longitude, and time difference and whether animal is different, 
  # (two records <30min apart of diff animals should be independent)
  duplicate_indices <-  dat$Latitude[-1] == dat$Latitude[-nrow(dat)] &
    
                        dat$Longitude[-1] == dat$Longitude[-nrow(dat)]&
    
                        abs(difftime(dat$datetime[-1], 
                                      dat$datetime[-nrow(dat)], 
                            units = "mins")) <= 30 &
    
                        dat$Common[-1] == dat$Common[-nrow(dat)]
  
  # set independence based on the duplicate indices 
  dat$newIndependent <- 1 #default to 1
  dat$newIndependent[duplicate_indices] <- 0 #if duplicate then 0
  
  
  # Return the modified data frame
  return(dat) #return dataframe with new independence  column
}

#>> Convert text to binary classification <<

##Get unique values
#unique(Master$Independent)#   NA  "No"  "Yes" "NO"  "YES" "yes" "no" 0 1

no<- c("No", "NO", "no")
yes<- c("YES", "yes", "Yes")

master<- Master

#if no then 0, else if yes then 1, else original value
master$Independent<- ifelse(Master$Independent %in% no, 0, ifelse(Master$Independent %in% yes, 1, Master$Independent))


#>> Run function on entire df for new column = "newIndependent" <<

master<- test_independence(master)


##what columns I want to see
#columns<- c("Species", "Survey.Name", "Common", "datetime", "Independent", "newIndependent", "Latitude" , "Longitude")
#View(master[, which(colnames(master) %in% columns)])

#set the columns of ```survey name %in% sites``` to NA to see if they change with next step
#master$Independent[master$Survey.Name %in% sites]<- NA 
# master$Nas<- rep(NA, nrow(master))#test column

#>> Apply newIndependent to Indepenendent column <<

#for the "independent" column for rows with "survey.name" in "sites" vector, change values to those of the "newIndependent" column for rows where "Survey.Name" is in "sites"
master$Independent[master$Survey.Name %in% sites] <- master$newIndependent[master$Survey.Name %in% sites]
master<- master[, -which(colnames(master) %in% c("newIndependent", "Nas"))]

#>> Write csv <<

write.csv(master, "Tapir Research/Code and Data/all Tapir's data/Costa Rica (Baird Tapir)/CR_Master(2023-7-10).csv")
