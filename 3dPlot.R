rm(list = ls())

library(ggplot2)
require(rgl)
require(lubridate)


setwd("C:/Users/chris/Documents/Research")
df<- read.csv("Tapir Research/Code and Data/all Tapir's data/Malaysia (Malayan Tapir)/Ma_T_Final_Covs.csv")
df<- na.omit(df)

#small.df <- df[sample(nrow(df), 2000), ]





x<- as.numeric(df$Precip)
y<- as.numeric(df$Elev)
z<- as.numeric(df$d.Road)

data<- data.frame(x,y,z)


labs<- c("precip",  "elev", "d.road")
names(data)<- labs
open3d()
#---------------------------------------

x<- seq(1, 10, length.out = 20)
y <- 1/(1+exp(1)^x)#x
f <- function(x, y) { r <- 1/(1+exp(1)^x);r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

# Draw the surface twice:  the first draws the solid part, 
# the second draws the grid.  Offset the first so it doesn't
# obscure the lines.
myplot<- function(x,y,z){
  par3d(windowRect = c(20, 30, 800, 800))
  bgplot3d(color = "white", expression = NULL)
  persp3d(x, y, z, aspect = c(1, 1, 0.5), col = "lightblue",
      xlab = "X", ylab = "Y", zlab = deparse(f), 
      polygon_offset = 1)
  persp3d(x, y, z, front = "lines", back = "lines", 
      lit = FALSE, add = TRUE)
}
myplot(x,y,z)
#---------------------------------------------


plot3d(x, y, z, xlab = labs[1], ylab = labs[2], zlab= labs[3], type = 's', 
       size = .75, lit = FALSE)

surface3d(x,y,z, alpha= .4, front = "lines", back = "lines")
#bbox3d(col= "white", emission= "green")
axes3d()


#given a model, predict xyz
#res = 16x16 grid
predictgrid<- function(model, x, y, z, res = 16, type= NULL){
  #find the ranges of the predictor variable
  xrange<- range(model$model[[xvar]])
  yrange<- range(model$model[[yvar]])
  newdata<- expand.grid(x = seq(xrange[1], xrange[2], length.out = res), 
                        y= seq(yrange[1], yrange[2], length.out = res))
  names(newdata)<- c(xvar, yvar)
  newdata[[zvar]] <- predict(model, newdata= newdata, type= type)
  newdata
}

df2mat <- function (p, xvar= NULL, yvar = NULL, zvar= NULL){
  if(is.null(xvar)) xvar<- names(p)[1]
  if(is.null(yvar)) yvar<-names(p)[2]
  if(is.null(zvar)) zvar<- names(p)[3]
  
  x<- unique(p[xvar])
  y<- unique(p(yvar))
  z<- matrix(p[[zvar]], nrow = length(y), ncol = length(x))
  
  m<- list(x,y,z)
  names(m) <- c(xvar, yvar,zvar)
  m
}

interleave<- function(v1,v2) as.vector(rbind(v1,v2))


mod<- lm(precip ~ elev + d.road + elev:d.road, data= data)

data$pred_precip<- predict(mod)

precipgrid_df<- predictgrid(mod, "precip",  "elev", "d.road")

precipgrid_list<- df2mat(precipgrid_df)

plot3d(data$precip, data$elev, data$d.road, type= "s", size = .5, lit = FALSE)

