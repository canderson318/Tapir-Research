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
lowland.raw<- read.csv("all Tapir's data/Amazon (Lowland Tapir)/AM_Records-Dataset01_CEP-GBR.csv", comment.char = '#')
baird.raw<- read.csv("all Tapir's data/Costa Rica (Baird_s Tapir)/Master(5.31.22).csv")
#remove rows with nas
lowland.raw<- na.omit(lowland.raw)
baird.raw<- na.omit(baird.raw)

#pull out neccessary columns for each df
baird<-data.frame(
  "date" = baird.raw$Date,
  "lat" = baird.raw$Latitude,
  "lon" = baird.raw$Longitude,
  "hour" = baird.raw$Time,
  "species"= baird.raw$Species
)

lowland<-data.frame(
  "date" = lowland.raw$date,
  "lat" = lowland.raw$lat,
  "lon" = lowland.raw$long,
  "hour" = lowland.raw$hour,
  "species" = lowland.raw$species
)

lowland<- lowland[lowland$species == "anta" , ]
baird<- baird[baird$species == "Tapirus bairdii" , ]

#check if df headers match requirments (date, species, hour, lat, lon)
  # checkHead<- function(df){
  #   headers<- c('hour', 'lat', 'lon', 'date', 'species')
  #   if (!all(headers %in% names(df))){
  #     print(paste(headers[!headers %in% names(df)], "not in data.frame"))
  #   } else{
  #     print("Correct columns present in data.frame")
  #   }
  # }
  # 
  # checkHead(lowland)
  # checkHead(baird)

##rename long to lon
  # names(lowland)[names(lowland)== 'long']<- "lon"
  # 
  # #replace Baird column names with correct names
  # old.names<- c('Date', "Time", "Latitude", "Longitude", "Species")
  # new.names<- c('date', 'hour', 'lat', 'lon', 'species')
  # col.nums<- match(old.names, colnames(baird)) #column numbers to update
  # colnames(baird)[col.nums] <- new.names #change column names to new column names

#function creates time and date objects with given date and time formats and converts time stamp to radian time
cleanTimeDate<- function(df, dateFormat, timeFormat){
  #change date and time formats
  df$date<- base::as.Date(df$date, format= dateFormat)
  df$hour <- format(df$hour, format = timeFormat)
  
  #convert h:m:s to radian as a numerical vector
  df$hour<- hms2rad(h=df$hour)
  return(df)
}

lowland<- cleanTimeDate(lowland, dateFormat = '%d/%m/%Y', timeFormat= "%H:%M")
baird<- cleanTimeDate(baird, dateFormat = "%m/%d/%Y", timeFormat = '%H:%M:%S')


  

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

sunData.lowland <- data.frame(
  "date" = lowland$date,
  "lat" = lowland$lat,
  "lon" = lowland$lon,
  "hour" = lowland$hour,
  "species" = lowland$species
)
sunData.baird<- data.frame(
  "date" = baird$date,
  "lat" = baird$lat,
  "lon" = baird$lon,
  "hour" = baird$hour,
  "species" = baird$species
)

sunData.lowland["solar"] <- solarhour(sunData.lowland, tzone=  'America/Porto_Velho')
sunData.baird["solar"] <- solarhour(sunData.baird, tzone= 'America/Costa_Rica')
# Save as CSV of each df
write.csv(sunData.lowland, "lowlandSunData.csv", row.names = FALSE)
write.csv(sunData.baird, "bairdSunData.csv", row.names = FALSE)
