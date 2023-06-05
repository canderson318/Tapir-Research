#This program plots the temporal density of two animals in an overlap plot. The data 
#must be in radian time and have radian solar time as well(see temporalDensity scripts)


#Christian Anderson, 6/2/2023
rm(list= ls())
library(data.table)
library(overlap)
library(dplyr)
library(sp)
setwd("C:/Users/chris/Documents/Research")
baird<- read.csv("temporalDensity/sunData/bairdSunData.csv")
lowland<- read.csv("temporalDensity/sunData/lowlandSunData.csv")

baird.solar<- baird$solar
lowland.solar<- lowland$solar
args<- list(
  animalname1= "Baird's Tapir",
  animalname2= "Lowland Tapir",
  animal1= baird.solar,
  animal2= lowland.solar
)

overlapPlotCustom <- function (A, B, 
                               xscale = 24, 
                               xcenter = c("noon", "midnight"),
                               linetype = c(1, 2), 
                               linecol = c("black", "blue"), 
                               linewidth = c(2,2), 
                               olapcol = "lightgrey", 
                               rug = FALSE, 
                               extend = NULL,  
                               n.grid = 128, 
                               kmax = 3, 
                               adjust = 1,
                               nightShade = TRUE, 
                               xaxsSelect="i", ...) 
{
  isMidnt <- match.arg(xcenter) == "midnight"
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale)) 
    1
  else xscale/(2 * pi)
  if (is.null(extend)) {
    xxRad <- seq(0, 2 * pi, length = n.grid)
  }
  else {
    xxRad <- seq(-pi/4, 9 * pi/4, length = n.grid)
  }
  if (isMidnt) 
    xxRad <- xxRad - pi
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  dots <- list(...)
  if (length(dots) == 1 && class(dots[[1]]) == "list") 
    dots <- dots[[1]]
  defaultArgs <- list(main = paste(deparse(substitute(A)), 
                                   "and", deparse(substitute(B))), xlab = "Time", ylab = "Density", 
                      bty = "o", type = "l", xlim = range(xx), ylim = c(0, 
                                                                        max(densA, densB)), font.axis=2)
  useArgs <- modifyList(defaultArgs, dots)
  selPlot <- names(useArgs) %in% c(names(as.list(args(plot.default))), 
                                   names(par(no.readonly = TRUE)))
  plotArgs <- useArgs[selPlot]
  plotArgs$x <- 0
  plotArgs$y <- 0
  plotArgs$type <- "n"
  plotArgs$xaxt <- "n"
  plotArgs$xaxs <- xaxsSelect #custom addition. "i" means no white space between data
  
  do.call(plot, plotArgs, quote = TRUE)
  polygon(c(max(xx), min(xx), xx), c(0, 0, densOL), border = NA, 
          col = olapcol)
  edge <- par("usr")
  if (nightShade){
    rect(edge[1],edge[3],6,edge[4], col=rgb(105/255, 167/255, 203/255,0.6),border=NA) #custom addition to color hours before dawn. 0.6 transparency (changed to 0.4 for overlap)
    rect(18,edge[3],edge[2],edge[4], col=rgb(105/255, 167/255, 203/255,0.6),border=NA) #custom addition to color hours after sunset. 0.6 transparency (changed to 0.4 for overlap)
  }
  
  if (!is.null(extend)) {
    if (isMidnt) {
      wrap <- c(-pi, pi) * xsc
    }
    else {
      wrap <- c(0, 2 * pi) * xsc
    }
    edge <- par("usr")
    rect(c(edge[1], wrap[2]), rep(edge[3], 2), c(wrap[1], 
                                                 edge[2]), rep(edge[4], 2), border = NA, col = extend)
    box(bty = useArgs$bty)
  }
  segments(xx[1], 0, xx[n.grid], 0, lwd = 0.5)
  lines(xx, densA, lty = linetype[1], col = linecol[1], lwd = linewidth[1])
  lines(xx, densB, lty = linetype[2], col = linecol[2], lwd = linewidth[2])
  if (rug) {
    if (isMidnt) {
      A <- ifelse(A < pi, A, A - 2 * pi)
      B <- ifelse(B < pi, B, B - 2 * pi)
    }
    axis(1, at = A * xsc, labels = FALSE, tcl = 0.35, lwd = 0, 
         lwd.ticks = 0.5, col = linecol[1])
    axis(1, at = B * xsc, labels = FALSE, tcl = -0.35, lwd = 0, 
         lwd.ticks = 0.5, pos = 0, col = linecol[2])
  }
  return(invisible(data.frame(x = xx, densityA = densA, densityB = densB)))
}

plotOverlap <- function(animalname1,animalname2, animal1, animal2) {
  par(ps=40, lwd=1.5, mar=c(6.5,8.5,5,2), cex.axis=0.7, mgp=c(5,1.5,0))#3rd 3
  overlapPlotCustom(
    animal1, animal2
    ,main=""
    ,xlab="Time of Day"
    ,ylab="Temporal Density"
    ,rug=TRUE
    ,xaxt="n"#remove axis labels so rewrite with custom
    ,yaxt="n"#remove axis labels so rewrite with custom
  )
  #add x-axis so labels within boundaries of plot and spaced
  axis(1, at=c(0), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=0) 
  axis(1, at=c(6, 12, 18), labels=c("      sunrise", "noon", "sunset      "), cex.axis=xaxisSize, font=1, hadj=0.5) 
  axis(1, at=c(24), labels=c("midnight"), cex.axis=xaxisSize, font=1, hadj=1)
  
  #add y-axis so all tickmarks and choose which labels
  axis(2,at=axTicks(2),labels=rep("",length(axTicks(2))),cex.axis=yaxisSize,font=1)#all tick marks
  axis(2,at=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],labels=axTicks(2)[seq(from=0,to=length(axTicks(2)),by=2)],cex.axis=yaxisSize,font=1)#every other label
  #axis(2,at=axTicks(2)[c(1,length(axTicks)-1)],labels=axTicks(2)[c(1,length(axTicks)-1)],cex.axis=yaxisSize,font=2)#only first and last
  
  
  par(xpd=TRUE)
  legend("topleft", legend = c(paste0(ind.data[ind.data$Species == animalname1, "Common"][1]," n=",length(animal1)), paste0(ind.data[ind.data$Species == animalname2, "Common"][1]," n=",length(animal2))), bty="n", pt.lwd=0.7,seg.len=5, col=c("black", "blue"), lty=c(1,2), cex=legendSize, text.font=1, y.intersp=2,inset=c(0,-0.1), ncol=2,lwd=3, text.width=c(0.5+strwidth(paste0(ind.data[ind.data$Species == animalname1, "Common"][1]," n=",length(animal1)),units="user", cex=legendSize)))
  
  overlapText <- paste0("=",sprintf("%.2f",round(overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1],2)))
  
  mtext(expression(Delta), side=3,adj=1,cex=textSize, font=1,at=24-strwidth(overlapText,units="user", cex=textSize))#21.1
  mtext(paste0(overlapText),side=3,adj=1,cex=textSize, font=1,at=24)
  
  dev.off()
}

overlapPlotCustom(A= args$animal1, B= args$animal2)

overlapPlot(args$animal1, args$animal2)

