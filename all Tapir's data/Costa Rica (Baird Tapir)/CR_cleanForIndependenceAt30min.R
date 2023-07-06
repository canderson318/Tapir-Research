# removes records that occur within the same hour at a given location
# written with chatGPT
#Christian Anderson 6/5/2023 edited: 7/05/2023
#------------------------------------------------------------------------------------------------
rm(list= ls())
#Required packages

require("lubridate")



setwd("C:/Users/chris/Documents/Research/Tapir Research/Code and Data/all Tapir's data")

#dataframe needs"lat", "lon", "date" (Y-M-D), and "hour" (H:M) column headers
#the function wants a 'species' column to work
raw<- read.csv("Costa Rica (Baird Tapir)/Master_CR(2023-7-6).csv", comment.char = '#')

#change independent values to binary

  #Values that exist in column
unique(raw$Independent)#   NA  "No"  "Yes" "NO"  "YES" "yes" "no" 

no<- c("No", "NO", "no")
yes<- c("YES", "yes", "Yes")

#if no then 0, else if yes then 1, else original value
raw$Independent<- ifelse(raw$Independent %in% no, 0, ifelse(raw$Independent %in% yes, 1, raw$Independent))

#rows with no independence value
unique(raw$Survey.Name[is.na(raw$Independent) ]) # surveys "Kamuk 2021 (March-July)" and "Kamuk 2021 (July-February)"


#format dates to date class given the df's month/day/full-year-number format
raw$Date<- base::as.Date(raw$Date, format = "%m/%d/%Y") #tryFormats= c("%m-%d-%Y", "%m-%d-%y", "%m/%d/%Y", "%m/%d/%y", "%y-%m-%d", "%Y-%m-%d", "%y/%m/%d", "%Y/%m/%d"))

#check for what type of formats in date and time
#print(unique(guess_formats(raw$Date, c("Ymd", "mdY", "ymd", "dmY", "dmy")))) 
  #"%Y-%Om-%d" "%Y-%m-%d" 

#make date time column
raw$datetime <- as.POSIXct(paste(raw$Date, raw$Time), tz = "America/Costa_Rica",format = "%Y-%m-%d %H:%M:%S")

#order by datetime
df<- raw[order(raw$datetime), ]


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

# dftest<- test_independence(df[(nrow(df) - 300):nrow(df),])
# View(dftest[,c(5,ncol(dftest), which(colnames(df) == "datetime"))])

# Call the test_independence function on the data frame for surveys in 2021, these were the ones with NAs
newKamuk<- test_independence(df[ df$Survey.Name == "Kamuk 2021 (March-July)" | df$Survey.Name == "Kamuk 2021 (July-February)"  ,  ])
newKamuk$Independent<- newKamuk$newIndependent
newKamuk<- newKamuk[,-which(colnames(newKamuk) == "newIndependent")]

#add new Independence values to independent column of df where image.ID matches for both dataframes

new_df<- df
new_df[new_df$Survey.Name %in% newKamuk$Survey.Name,] <- newKamuk
write.csv(new_df, "Costa Rica (Baird Tapir)/Master_CR(2023-7-6)-2.csv")
#View(df[,c(5,ncol(df), which(colnames(df) == "datetime"))])
