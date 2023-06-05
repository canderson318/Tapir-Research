##Christian Anderson, 5/30/2023, 
##Convert H:M:S string time stamps to radian numerical vector and plot kernal density plot in Overlap
rm(list= ls())
require(overlap)
library(astroFns)
require(ggplot2)


df<- read.csv("all Tapir's data/Amazon (Lowland Tapir)/Combining_AMDatasets/Species.Record.table.RDSA.csv")

##subset Tapir
#tapir<- subset(df, df$Common == "Baird's Tapir")

#extract hour char vector
time<- df$hour

#convert h:m:s to radian and numerical vector
time.rads<- hms2rad(h=time)




plot<- densityPlot(time.rads, 
                   main= "Circadian Density plot of Lowland Tapir",
                   ylab = "Density",
                   xscale= 24,
                   add = FALSE, 
                   rug = TRUE, 
                   extend = NULL,
                   n.grid = 128, 
                   kmax = 3, 
                   adjust = 1,
                   xaxt= 'n')
sunrise<- 6.25
sunset<- 18.25



#shade nighttime
polygon(c(0,sunrise, sunrise, 0), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA)
polygon(c(sunset,24, 24, sunset), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA)

polygon()
#add labels
categories<- c('midnight', 'sunrise', 'noon', 'sunset', 'midnight')
cat.x <- c(0, sunrise, 12, sunset, 24)
axis(1, at = cat.x, labels = categories)

