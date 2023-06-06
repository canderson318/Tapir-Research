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
df["hour.rad"]<- hms2rad(h=df$hour)

#_________________________________________________________________________________

#inputs: 'dat' is a data.frame with the following columns: "date" (the POSIXct date), "lat" (the Latitude), "lon" (the Longitude), "time" (the time of day in RADIANS)
#ouptuts: 'solar' is a vector of "solar times" (in RADIANS) where (1/2)pi is sunrise and (3/2)pi is sunset

#Get sunrise and sunset as date-time objects
sunData <- getSunlightTimes(data = df, keep = c("sunrise", "sunset"), tz = args$tzone)
sunRise <- sunData$sunrise
sunSet <- sunData$sunset

#Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
sunRise <- time_length(interval(start = ymd_hms(paste(date(sunRise), "00:00:00"), tz = args$tzone),
                                end = force_tz(time = sunRise, tzone = args$tzone)),
                       unit = "day")
sunSet <- time_length(interval(start = ymd_hms(paste(date(sunSet), "00:00:00"), tz = args$tzone),
                               end = force_tz(time = sunSet, tzone = args$tzone)),
                      unit = "day")
# plot(sunRise, ylim = c(0,1), pch = ".")
# plot(sunSet, ylim = c(0,1), pch = ".")

#Convert sunrise/sunset to radians
sunRise <- sunRise * 2 * pi
sunSet <- sunSet * 2 * pi
# plot(sunRise, ylim = c(0, 2*pi), pch=".")
# plot(sunSet, ylim = c(0,2*pi), pch=".")

clockTime <- df[["hour.rad"]]
solar <- rep(NA, times = length(clockTime))

for (i in 1:length(clockTime)) {
  if (clockTime[i] <= sunRise[i]) {
    solar[i] <- ((1/2)*pi) * (clockTime[i]/sunRise[i]) #Predawn observations
  } else if (clockTime[i] <= sunSet[i]) {
    solar[i] <- (((clockTime[i] - sunRise[i])/(sunSet[i] - sunRise[i]))*pi) + ((1/2)*pi) #Daylight observations
  } else {
    solar[i] <- (((clockTime[i] - sunSet[i])/((2*pi) - sunSet[i]))*(1/2)*pi) + ((3/2)*pi) #Postdusk observations
  }
}

#______________________________________________________________________________________________
#make a new df 
solar.df <- data.frame(
  "date" = df$date,
  "lat" = df$lat,
  "lon" = df$lon,
  "hour" = df$hour,
  "radian.hour" = df$hour.rad,
  "datetime"= df$datetime,
  "solar" = solar
)

write.csv(solar.df, 
          "C:/Users/chris/Documents/Research/Code and Data/TapirResearch/temporalDensity/sunData/malayanSunData.csv", 
          row.names = FALSE)


