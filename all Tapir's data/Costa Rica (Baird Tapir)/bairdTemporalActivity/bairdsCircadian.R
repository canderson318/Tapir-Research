##Christian Anderson, 5/30/2023, 
##Convert H:M:S string time stamps to radian numerical vector and plot kernal density plot in Overlap
rm(list= ls())

require(overlap)
library(astroFns)
require(ggplot2)

setwd("C:/Users/chris/Documents/Research")

df<- read.csv("all Tapir's data/Costa Rica (Baird_s Tapir)/CR_Dataset04_Lowland.csv")

#filter for tapir 
tapir<- subset(df, df$Common == "Baird's Tapir")

#extract hour char vector
time<- tapir$Time

#convert h:m:s to radian and numerical vector
time.rads<- hms2rad(h=time)


# Shade the region


plot<- densityPlot(time.rads, 
                   main= "Circadian Density plot of Baird's Tapir",
                   ylab = "Density",
                   xscale= 24,
                   add = FALSE, 
                   rug = TRUE, 
                   extend = NULL,
                   n.grid = 128, 
                   kmax = 3, 
                   adjust = 1,
                   xaxt= 'n')

#shade night
#sunrise/sunset: 6.1/18.1
polygon(c(0,6.1, 6.1, 0), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA, type= 'n') #left

polygon(c(18.1,24, 24, 18.1), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA, type= 'n')#right



#add labels

categories<- c('midnight', 'sunrise', 'noon', 'sunset', 'midnight')
cat.x <- c(0, 6, 12, 18, 24)
x.labels<- axis(1, at = cat.x, labels = categories)

