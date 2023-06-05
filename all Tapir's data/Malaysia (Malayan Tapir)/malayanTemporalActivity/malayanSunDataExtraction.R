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


setwd("C:/Users/chris/Documents/Research/Code and Data/TapirResearch")

#dataframe needs"lat", "lon", "date" (Y-M-D), and "hour" (H:M) column headers
#the function wants a 'species' column to work
df<- read.csv("all Tapir's data/Malaysia (Malayan Tapir)/TapirDataMalaysia_2009-2011_(independent_records_at_1hr).csv",
                  comment.char = '#')

#list arguments
args<- list(df= df,
            patternAnimal= "Tapirus indicus", #name in dataframe under species 
            commonName= "Malayan Tapir",#common name for figure
            tzone= 'Asia/Kuala_Lumpur',#timezone of study site
            dateFormat = "%m/%d/%Y",#try "%y-%m-%d" #Formats for time and date
            timeFormat = "%H:%M")

#clean time and date

#change date and time formats
df$date<- substr(df$datetime, 1,10)
df$date<- base::as.Date(df$date, tryFormats= c("%m-%d-%Y", "%m-%d-%y", "%m/%d/%Y", "%m/%d/%y",
                                               "%y-%m-%d", "%Y-%m-%d", "%y/%m/%d", "%Y/%m/%d"))
df$hour <- format(df$hour, format = args$timeFormat)

#convert h:m:s to radian as a numerical vector
df$hour<- hms2rad(h=df$hour)

#Solar data function 
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
  sunData <- getSunlightTimes(data= dat, keep = c("sunrise", "sunset"), tz = tzone)
  sunRise <- sunData$sunrise
  sunSet <- sunData$sunset
  
  #Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
  #make sunrise and sunset intervals of time as a ratio of a day 
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

#make a new df 
sunData <- data.frame(
  "date" = df$date,
  "lat" = df$lat,
  "lon" = df$lon,
  "hour" = df$hour,
  "datetime"= df$datetime
)



sunData["solar"] <- solarhour(df, tzone= args$tzone)

#write.csv(sunData, "C:/Users/chris/Documents/Research/Code and Data/TapirResearch/temporalDensity/sunData/malayanSunData.csv", row.names = FALSE)




