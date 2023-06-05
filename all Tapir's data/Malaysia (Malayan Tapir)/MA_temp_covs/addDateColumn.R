#This script adds a month column to each month temp covariate csv file by 
#extracting the month digit from the first two chars of the file path, 
#e.g., "01" for 01_max_min_temp_MA.csv
#Christian Anderson, 5/31/2023
rm(list= ls())

library(dplyr)
setwd("C:/Users/chris/Documents/Research/all Tapir's data/Malaysia (Malayan Tapir)/MA_temp_covs/combinedtempAvgs")


createMonthCol<- function(filename){
  file<- read.csv(filename)
  
  monthchar<- substr(filename, start= 1, stop= 2) #extracts the month digits
  month<- as.numeric(monthchar)#makes numeric
  
  file["Month"]<- month

#  tail<- "_maxmin_temp_MA.csv"
  
#  newfilename<- paste(monthchar, tail, sep= '')#rename file to something different
  
  write.csv(file, file = filename, row.names = FALSE) 
  
}

# text document of listing from terminal command " dir /B > [filename].txt " to make a 
# text file of the file paths within the directory. These are then used in the for loop
# where the function is applied to them
directory<- readLines('directory.txt')

for (file in directory) {
  createMonthCol(file)
}

