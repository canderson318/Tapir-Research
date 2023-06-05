rm(list=ls())
setwd("~/Desktop/ Honor's Project/Malayan Tapir Data")
dir()

dat<- read.csv("TapirDataMalaysia_2009-2011.csv", header=TRUE)
head(dat)
dim(dat)

#############################################################################
### get independent records table ###########################################

photo.tab <- dat 		# Insert your tables with records here!
head(dat)
dat$Date

table(dat$Species)

#extract only necessary columns, remove records with NA as date
dat1<- cbind(dat, ctid=paste(dat$Site,dat$Camr_Nm))
head(dat1)

#extract only necessary columns, remove records with NA as date
recs<-dat1[!is.na(dat1$Date),c("ctid", "Species", "Date", "Time")]
dim(recs)
head(recs)

#create stations list
stations<-unique(recs$ctid)
stations
J<- length(stations)
species<-sort(unique(recs$Species))

recs.all<-recs[1,] #set up structure of independent record table

for (i in 1:length(species)){
  for (j in 1:J){
    sub<- recs[recs$ctid==stations[j] & recs$Species == species[i],]
    sub<- na.omit(sub)
    if(dim(sub)[1]==0) next
    
    ordsub<- order(strptime( paste(sub$Date, sub$Time), "%m/%d/%Y %H:%M"))            
    sub2<- sub[ordsub,]
    
    dd<-unique(sub2$Date)
    
    dt<-strptime( paste(sub2$Date, sub2$Time), "%m/%d/%Y %H:%M")
    nr<-length(dt)
    
    recs.all<-rbind(recs.all, sub2[1,]) #always add first record
    
    if(dim(sub2)[1]>1) { #if more than one, check for independence
      
      for (k in 2:nr){
        dif<-difftime(dt[k], dt[k-1], units="mins")
        if(dif>(24*60)) {recs.all<-rbind(recs.all,sub2[k,] )}
      }
    }
  }
}
#remove starter row
recs.all <- recs.all[-1,]
dim(recs.all)

#####################################################################################
### Create table with the number of independent records per camera per month/year ####

# Subseting dataset per ct
#stations <- recs.all$ctid

stations_recs<- list()
for(c in 1:length(stations)){
  stations_recs[[c]]<-  recs.all[ recs.all$ctid== stations[c],]
  names(stations_recs)[c]<- stations[c]
}
str(stations_recs)
length(stations_recs)
stations_recs[[2]]
length(stations)

m.y<- list()
for(ct in 1:length(stations)){
  date1<-  as.Date(stations_recs[[ct]][,"Date"], "%m/%d/%Y", na.rm=T)#
  y<- data.frame(Year = as.numeric(format(date1, "%Y")),
                 Month= as.numeric(format(date1, "%m")))
  m.y[[ct]]<- table(y[,"Month"],y[,"Year"])
}
m.y

range(as.Date(recs.all[,"Date"], "%m/%d/%Y", na.rm=T))

t2009<- matrix(NA, length(stations), 12)
rownames(t2009)<- stations
colnames(t2009)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2009",12))

t2010<- matrix(NA, length(stations), 12)
rownames(t2010)<- stations
colnames(t2010)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2010",12))

t2011<- matrix(NA, length(stations), 12)
rownames(t2011)<- stations
colnames(t2011)<- paste(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec"),rep("2011",12))


class(m.y)
ncol(m.y[[70]])

ncol(m.y[[116]])

for(ct in 1:length(stations)){
  for(c in 1:ncol(m.y[[ct]])){
    if(colnames(m.y[[ct]])[c]=="2009"){t2009[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
    if(colnames(m.y[[ct]])[c]=="2010"){t2010[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 		
    if(colnames(m.y[[ct]])[c]=="2011"){t2011[ct,as.numeric(rownames(m.y[[ct]]))]<- m.y[[ct]][,c]} 
  }
}


#output csv
d2.CT.on<- cbind(t2009,t2010,t2011)
d2.CT.on[d2.CT.on==0]<- NA
d2.CT.on
#write.csv(d2.CT.on, "CR_d2.CT.IndRecs3.csv")

