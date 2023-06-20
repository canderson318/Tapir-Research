rm(list= ls())
#Required packages
require("overlap")
require("suncalc")
require("lubridate")
require("astroFns")

tzone<- "America/Los_Angeles"
sunrise<- as.POSIXct( "2023-06-06 05:41:00")
sunset<- as.POSIXct("2023-06-06 19:53:00")
            
            
#ratio of day from midnight to sunrise
rise.ratio <- time_length(interval(start = ymd_hms(paste(date(sunrise), "00:00:00"), tz = tzone),
                                end = force_tzs(sunrise, tzone)),
                        unit = "day")

#ratio of day from sunset to midnight
set.ratio <- time_length(interval(start = ymd_hms(paste(date(sunset), "00:00:00"), tz = tzone),
                               end = force_tzs( sunset, tzone)),
                       unit = "day")

rise.rad<- rise.ratio * 2 * pi#radian time of darkness at begining of new day 
set.rad<- set.ratio * 2 * pi#radian time of darkness at end of day 

times<- c("03:15:00", "16:22:00", "23:44:00", "12:00:00")


times.rad<- hms2rad(format(times, format = "%H:%M"))

time<- cbind(times, times.rad)

i=3
solar.at3<- (((times.rad[i] - set.rad)/((2*pi) - set.rad))*(1/2)*pi) + ((3/2)*pi)

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