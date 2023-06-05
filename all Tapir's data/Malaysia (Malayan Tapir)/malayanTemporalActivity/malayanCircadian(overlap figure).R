##Convert H:M:S string time stamps to radian numerical vector and plot kernal density plot in Overlap
##Christian Anderson, 5/25/2023, 
rm(list= ls())
require(overlap)
library(astroFns)
require(ggplot2)

setwd("C:/Users/chris/Documents/Research")
malay.times<- read.csv("all Tapir's data/Malaysia (Malayan Tapir)/recs.all_MalayanTapir.csv", 
                       sep = ",", 
                       comment.char = "#", 
                       header = TRUE)

#extract hour char vector
time<- malay.times$hour

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
                          main= "Circadian density plot of Malayan Tapir",
                          ylab = "Density",
                          xscale= 24,
                          add = FALSE, 
                          rug = TRUE, 
                          extend = NULL,
                          n.grid = 128, 
                          kmax = 3, 
                          adjust = 1,
                          xaxt= 'n')
#Shade nighttime
polygon(c(0,7, 7, 0), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA)
polygon(c(19,24, 24, 19), c(0, 0, 1, 1), col = alpha("blue", .3), border = NA)

#add categorical labels
categories<- c('midnight', 'sunrise', 'noon', 'sunset', 'midnight')
cat.x <- c(0, 7, 12, 19, 24)
axis(1, at = cat.x, labels = categories)



