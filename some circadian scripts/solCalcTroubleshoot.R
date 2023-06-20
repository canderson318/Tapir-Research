#possible issues with previous version: 
##date() depricated, use as.Date; force_tz needs time not hour = time and use force_tzs instead; 
##use hms2rad instead of messy math
#-------------------------------------------------------------------------------------
rm(list= ls())
#Required packages
require("overlap")
require("suncalc")
require("lubridate")
require("astroFns")




setwd("C:/Users/chris/Documents/Research/all Tapir's data/Amazon (Lowland Tapir)")

df<- read.csv("AM_Records-Dataset01_CEP-GBR.csv")

#Format date and time objects nicely
dateFormat = "%d/%m/%Y"
df$date<- base::as.Date(df$date, format= dateFormat)
#df$date<- as.POSIXct(df$date)

df$hour <- format(df$hour, format = "%H:%M")

#rename long to lon (accepted by suncalc methods)
names(df)[names(df) == "long"] <- "lon"

#get rid of nas (double check that date formatting is not trying to coerce the wrong format, 
#that creates nas)

df<- na.omit(df)


#convert h:m:s to radian as a numerical vector
df$hour<- hms2rad(h=df$hour)


#Get sunrise and sunset as date-hour objects
sunData <- getSunlightTimes(data = df, keep = c("sunrise", "sunset"), tz = 'America/Porto_Velho')
sunRise <- sunData$sunrise
sunSet <- sunData$sunset

start<- ymd_hms(paste(as.Date(sunRise), "00:00:00"), tz = 'America/Porto_Velho')
end<- force_tzs(sunRise, 'America/Porto_Velho')

interval<- interval(start= start, end= end)

#_________________________________________________________

sunData <- getSunlightTimes(data = dat, keep = c("sunrise", "sunset"), tz = tzone)
sunRise <- sunData$sunrise
sunSet <- sunData$sunset

#Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
sunRise <- time_length(interval(start = 
                                  ymd_hms(
                                    paste(
                                      date(sunRise), "00:00:00"), tz = tzone),
                                end = 
                                  force_tz(hour = sunRise, 
                                           tzone = tzone)),
                       unit = "day")
sunSet <- time_length(interval(start = 
                                 ymd_hms(
                                   paste(
                                     date(sunSet), "00:00:00"), tz = tzone),
                               end = 
                                 force_tz(hour = sunSet, 
                                          tzone = tzone)),
                      unit = "day")


#____________________________________________________________________________

#Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
sunRise <- time_length(interval(start = 
                                  ymd_hms(
                                    paste(
                                      as.Date(sunRise), 
                                      "00:00:00"), 
                                    tz = "UTC"),
                                end = 
                                  force_tz(hour = sunRise, 
                                           tzone = timezone)),
                       unit = "day")
sunSet <- time_length(interval(start = 
                                 ymd_hms(
                                   paste(
                                     as.Date(sunSet), 
                                     "00:00:00"), 
                                   tz = "UTC"),
                               end = 
                                 force_tz(hour = sunSet, 
                                          tzone = timezone)),
                      unit = "day")

