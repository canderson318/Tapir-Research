#R Version 4.3.0
#Converting "Clock hour" to "Solar hour"
#df must have a column for hour ("hourRad" -- in RADIANS) and columns for Longitude and Latitude, respectively

#Issues with previous version: 
##date() depricated, use as.Date; force_tz needs time not hour = time and use force_tzs instead; 
##use hms2rad instead of gross math

#Christian Anderson 6/1/2023


#-----------------------------------------------------------------------------------------------

#Directions: use data frame with lat, lon, date, and hour. The densPlot() function accepts the list of arguments, args; 
#after reading in the dataframe and changing the headers, these arguments ensure the correct time and date formats for the 
#data (this allows the as.date() function to work). Use OlsonNames() for appropriate timezones.
#Within densPlot() solarhour() creates a column of the solar time for that detections lat, lon, and radian time for that timezone.

#------------------------------------------------------------------------------------------------
rm(list= ls())
#Required packages
require("overlap")
require("suncalc")
require("lubridate")
require("astroFns")


setwd("C:/Users/chris/Documents/Research")

#dataframe needs"lat", "lon", "date" (Y-M-D), and "hour" (H:M) column headers
#the function wants a 'species' column to work
df<- read.csv("all Tapir's data/Peru (Mountain Tapir)/MOUNTAIN_Data Formatting - Global Tapir Project_JLM_final.xlsx - Data.csv")

#remove trail column
df<- df[,-which(names(df) == "Trail")]

df<- na.omit(df)

#check if df headers match requirments 
headers<- c('hour', 'lat', 'lon', 'date', 'species')
if (!all(headers %in% names(df))){
  print(paste(headers[!headers %in% names(df)], "not in data.frame"))
} else{
  print("Correct columns present in data.frame")
}

#replace column names with correct names
old.names<- c('Date', "Time", "Latitud", "Longitd", "Species")
new.names<- c('date', 'hour', 'lat', 'lon', 'species')
col.nums<- match(old.names, colnames(df)) #column numbers to update
colnames(df)[col.nums] <- new.names #change column names to new column names

#list arguments
args<- list(df= df,
            patternAnimal= "Tapirus pinchaque", #name in dataframe under species 
            commonName= "Mountain Tapir",#common name for figure
            timezone= 'America/Costa_Rica',#timezone of study site
            dateFormat = "%m/%d/%Y", #Formats for time and date
            timeFormat = "%H:%M:%S")


densPlot<- function(df, timezone, patternAnimal, commonName, dateFormat, timeFormat){
  
  df$date<- base::as.Date(df$date, format= dateFormat)
  df$hour <- format(df$hour, format = timeFormat)
  
  #convert h:m:s to radian as a numerical vector
  df$hour<- hms2rad(h=df$hour)
  
  #Get Solar data function 
  
  solarhour <- function(dat, tzone) {
    #inputs: 
    #'dat' is a data.frame with the following columns: 
    #"date" (the POSIXct date), 
    #"lat" (the Latitude),
    #"lon" (the Longitude), 
    #"hour" (the hour of day in RADIANS)
    #ouptuts: 
    #'solar' is a vector of "solar hours" (in RADIANS) where (1/2)pi is sunrise and (3/2)pi is sunset
    
    #Get sunrise and sunset as date-hour objects
    sunData <- getSunlightTimes(data = dat, keep = c("sunrise", "sunset"), tz = tzone)
    sunRise <- sunData$sunrise
    sunSet <- sunData$sunset
    
    #Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
    sunRise <- time_length(interval(start = 
                                      ymd_hms(
                                        paste(
                                          as.Date(sunRise), "00:00:00"), tz = tzone),
                                    end = 
                                      force_tzs(sunRise, tzone)),
                           unit = "day")
    sunSet <- time_length(interval(start = 
                                     ymd_hms(
                                       paste(
                                         as.Date(sunSet), "00:00:00"), tz = tzone),
                                   end = 
                                     force_tzs( sunSet, tzone)),
                          unit = "day")
    # plot(sunRise, ylim = c(0,1), pch = ".")
    # plot(sunSet, ylim = c(0,1), pch = ".")
    
    #Convert sunrise/sunset to radians
    sunRise <- sunRise * 2 * pi
    sunSet <- sunSet * 2 * pi
    # plot(sunRise, ylim = c(0, 2*pi), pch=".")
    # plot(sunSet, ylim = c(0,2*pi), pch=".")
    
    clockhour <- dat[["hour"]]
    solar <- rep(NA, hours = length(clockhour))
    
    for (i in 1:length(clockhour)) {
      if (clockhour[i] <= sunRise[i]) {
        solar[i] <- ((1/2)*pi) * (clockhour[i]/sunRise[i]) #Predawn observations
      } else if (clockhour[i] <= sunSet[i]) {
        solar[i] <- (((clockhour[i] - sunRise[i])/(sunSet[i] - sunRise[i]))*pi) + ((1/2)*pi) #Daylight observations
      } else {
        solar[i] <- (((clockhour[i] - sunSet[i])/((2*pi) - sunSet[i]))*(1/2)*pi) + ((3/2)*pi) #Postdusk observations
      }
    }
    
    return(solar)
  }
  
  sunData <- data.frame(
    "date" = df$date,
    "lat" = df$lat,
    "lon" = df$lon,
    "hour" = df$hour
  )
  
  
  df["solar"] <- solarhour(sunData,timezone)
  
  densityPlot(
    A = subset(df$solar, df$species == patternAnimal ),
    xscale = NA,
    xlab = "Solar Position",
    xaxt = "n",
    main = paste(commonName,"Temporal Density"),
    extend= NULL
  )
  rug(
    x = subset(df$solar, df$species == patternAnimal),
    side = 1
  )
  #solar times
  rise<- pi/2
  set<- 3*pi/2
  midnight<- 2* pi
  
  #night shading
  polygon(c(0,rise, rise, 0), c(0, 0, 1, 1), col = rgb(0, 0, 1, alpha = 0.5), border = NA)
  polygon(c(set,midnight, midnight, set), c(0, 0, 1, 1), col = rgb(0, 0, 1, alpha = 0.5), border = NA)
  
  #labeling solar times
  axis(
    side = 1,
    at = c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")
  )
  
}

# Call the function using the arguments from the list
output <- do.call(densPlot, args)
