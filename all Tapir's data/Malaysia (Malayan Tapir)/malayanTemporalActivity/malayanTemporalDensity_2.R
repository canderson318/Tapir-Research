#------------------------------------------------------------------------------------------------
rm(list= ls())

require("overlap")
require("suncalc")
require("lubridate")
require("astroFns")


setwd("C:/Users/chris/Documents/Research/Code and Data/TapirResearch")




args<- list(
            patternAnimal= "Tapirus indicus", #name in dataframe under species 
            commonName= "Malayan Tapir",#common name for figure
            timezone= 'Asia/Kuala_Lumpur',#timezone of study site
            dateFormat = "%y-%m-%d", #Formats for time and date
            timeFormat = "%H:%M")


sunData<- read.csv("temporalDensity/sunData/malayanSunData.csv", comment.char = '#')
sunData$date <- substr(sunData$datetime, 1,10) #get the nicely formatted date

# formats<- list("%m-%d-%Y", "%m-%d-%y",
# "%y-%m-%d", "%Y-%m-%d", )

#make date "Date" type 
sunData$date<- base::as.Date(sunData$date, tryFormats= c("%Y-%m-%d"))

if(!class(sunData$date) == 'Date'){
  stop("date class incorrect")
}

densityPlot(
  A = sunData$solar,
  xscale = NA,
  xlab = "Solar Position",
  xaxt = "n",
  main = paste(args$commonName,"Temporal Density"),
  extend= NULL
)
rug(
  x = subset(sunData$solar, sunData$species == args$patternAnimal),
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

bw<- getBandWidth(sunData$solar, kmax= 3)
