# removes records that occur within the same hour at a given location
# written with chatGPT
#Christian Anderson 6/5/2023
#------------------------------------------------------------------------------------------------
rm(list= ls())
#Required packages
require("overlap")
require("suncalc")
require("lubridate")
require("astroFns")


setwd("C:/Users/chris/Documents/Research/Code and Data/TapirResearch")

#dataframe needs"lat", "lon", "date" (Y-M-D), and "hour" (H:M) column headers
#the function wants a 'species' column to work
df.raw<- read.csv("all Tapir's data/Malaysia (Malayan Tapir)/TapirDataMalaysia_2009-2011.csv", comment.char = '#')
#"C:\Users\chris\Documents\Research\all Tapir's data\Malaysia (Malayan Tapir)\TapirDataMalaysia_2009-2011.csv"


df<- data.frame(
  "species" = df.raw$Species,
  "lat"= df.raw$Latitud,
  "lon" = df.raw$Longitd,
  "hour"= df.raw$Time,
  "date" = df.raw$Date
  
)
df$date<- base::as.Date(df$date, tryFormats= c("%m-%d-%Y", "%m-%d-%y", "%m/%d/%Y", "%m/%d/%y",
                                               "%y-%m-%d", "%Y-%m-%d", "%y/%m/%d", "%Y/%m/%d"))

df$datetime <- as.POSIXct(paste(df$date, df$hour), format = "%y-%m-%d %H:%M")

#order by datetime
df<- df[order(df$datetime), ]

# Function to remove records within one hour of each other at a specific location
remove_duplicate_records <- function(data, lat, lon, datetime) {
  # Initialize empty list to store indices of records to remove
  remove_indices <- list()
  
  # Loop over each record
  for (i in 1:(nrow(data) - 1)) {
    # Check if the current record matches the next record's location
    if (data$lat[i] == data$lat[i+1] && data$lon[i] == data$lon[i+1]) {
      # Check if the time difference is within one hour
      if (abs(difftime(data$datetime[i], data$datetime[i+1], units = "hours")) <= 1) {
        # Add indices of duplicate records to the remove_indices list
        remove_indices <- c(remove_indices, i+1)
      }
    }
  }
  
  # Remove duplicate records from the data frame
  if (length(remove_indices) > 0) {
    data <- data[-unlist(remove_indices), ]
  }
  
  # Return the modified data frame
  return(data)
}

# Call the remove_duplicate_records function on the data frame
df_cleaned <- remove_duplicate_records(df, lat, lon, datetime)

#check for replicates
tab<- table(df_cleaned$datetime)
for(i in names(tab)){
  if(tab[i] >1){
    print(paste(i, "occured", tab[i], "times"))
    x<- TRUE
  }
}



# Print the cleaned data frame
write.csv(df_cleaned, 
          file="all Tapir's data/Malaysia (Malayan Tapir)/TapirDataMalaysia_2009-2011_(independent_records_at_1hr).csv", 
          row.names = FALSE)
