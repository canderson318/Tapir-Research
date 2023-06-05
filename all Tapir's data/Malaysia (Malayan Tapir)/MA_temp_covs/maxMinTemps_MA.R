#Combining min max temp covariate columns into one dataset for 12 months for the malayan tapir
#Christian Anderson 5/30/2023

rm(list = ls())

setwd("C:/Users/chris/Documents/Research/all Tapir's data/Malaysia (Malayan Tapir)/MA_temp_covs")

library(dplyr)


joinMaxTemp<- function(maximumTempCSV, minimumTempCSV){
  max<- read.csv(maximumTempCSV)
  min<- read.csv(minimumTempCSV)
  min['Avg.Max.Temp']<- max$Avg.Max.Temp1
  
  #rename temp columns
  names.new<- c("Min Temp (C)", "Min Temp (C)")
  names(min)[8:9] <- names.new
  
  #make file name and save as CSV file
  month<- substr(maximumTempCSV, start= 1, stop= 2) #extracts the month digits
  tail<- "_max_min_temp_MA.csv"
  file_name<- paste(month, tail, sep= '')
  
  write.csv(min, file = file_name, row.names = FALSE) 
  
}


# text document of listing from terminal command " dir /B > file.txt " to make a 
# text file of the file paths within the directory. These are then used in the for loop
# where the function is applied to them
fileList <- readLines("listing.txt")

# Iterate through each filename and call the function. 
#Every two because two files for each month
for (i in seq(1, 24, 2)) {
  nextfilename<- fileList[i+1]
  filename<- fileList[i]
  joinMaxTemp(filename, nextfilename)
}

