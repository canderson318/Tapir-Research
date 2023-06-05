##Christian Anderson, 5/30/2023, 
##Convert H:M:S string time stamps to radian numerical vector and plot kernal density plot in Overlap
rm(list= ls())
require(overlap)
library(astroFns)
require(ggplot2)


mountain<- read.csv("all Tapir's data/Peru (Mountain Tapir)/recs.all_MountainTapir.csv")

#extract hour char vector
time<- mountain$hour

#convert h:m:s to radian and numerical vector
time.rads<- hms2rad(h=time)

'''

#plot time radians in kernal density against its own data
plot.against_self<- overlapPlot(time.rads, time.rads, 
                                xcenter = c("noon", "midnight"),
                                rug= TRUE
)

density_estimate <- density(time.rads, bw= 0.3)

# Plot simple kernel density
plot.simple_kernel<- plot(density_estimate, 
                          main = "Kernel Density of Time Data(rads)", 
                          xlab = "Time", 
                          ylab = "Density")

'''



plot<- densityPlot(time.rads, 
                   main= "Circadian Density plot of Mountain Tapir",
                   ylab = "Density",
                   xscale= 24,
                   add = FALSE, 
                   rug = TRUE, 
                   extend = NULL,
                   n.grid = 128, 
                   kmax = 3, 
                   adjust = 1,
                   xaxt= 'n')

#Shade night
  #left bottom, right bottom, right top, left top for x and y respectively 
polygon(c(0,6.1, 6.1, 0), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA)
polygon(c(18.1,24, 24, 18.1), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA)


#add labels
categories<- c('midnight', 'sunrise', 'noon', 'sunset', 'midnight')
cat.x <- c(0, 6, 12, 18, 24)
axis(1, at = cat.x, labels = categories)
