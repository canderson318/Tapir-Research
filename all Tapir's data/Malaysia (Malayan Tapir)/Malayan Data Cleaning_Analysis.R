###=================================================================================###
### Script to clean Malayan Tapir Data and produce Community Detection Histories    ###
### Honor's Project - 2023	        							                                	###
### 15 March 2023 											                                            ###
### Emily Bohnet        										                                        ###
###=================================================================================###

#clear system
rm(list=ls())

#set wd
setwd("~/Desktop/ Honor's Project/Malayan Tapir Data")
dir()

#read in data
ma_dat<- read.csv("TapirDataMalaysia_2009-2011.csv")
#s <- read.csv("Data Formatting - Global Tapir Project_JLM_stations.csv")

#fix error in date format
#s$Cmr_E_D[34] <- "08/29/2016"
#mt_dat$Cmr_E_D[57] <- "08/29/2016"
#mt_dat$Cmr_E_D[58] <- "08/29/2016"

#unique stations - 28
stat<- sort(unique(ma_dat$Camr_Nm))
stat

#explore data
ma_dat$Date
table(ma_dat$Species)
colnames(ma_dat)
dim(ma_dat)

#create unique survey name
unique(paste(ma_dat$Srvy_Nm, ma_dat$Camr_Nm))
unique(paste(ma_dat$Site, ma_dat$Camr_Nm))
dat1<- cbind(ma_dat,paste(ma_dat$Site, ma_dat$Camr_Nm))
head(dat1)
#change column names
colnames(dat1)[6]<- "Camr_Nm1"
colnames(dat1)[17]<- "Camr_Nm"
unique(dat1$Camr_Nm) #there are 28 unique survey names

#combine station names for stations
s<- cbind(ma_dat,paste(ma_dat$Site, ma_dat$Camr_Nm))
head(s)
#change column names
colnames(s)[6]<- "Camr_Nm1"
colnames(s)[15]<- "Camr_Nm"

#extract only necessary columns, remove records with NA as date - no NAs in this dataset
recs<-dat1[!is.na(dat1$Date),c("Camr_Nm", "Species", "Date", "Time")]
recs_loc <- dat1[!is.na(dat1$Date),c("Camr_Nm", "Species", "Date", "Time", "Latitud", "Longitd")]
dim(recs)


##########################################################################################################
### Independent Records ############################
stations<-unique(recs$Camr_Nm)

J<- length(stations)
species<-sort(unique(recs$Species))

recs.all<-recs[1,] #set up structure of independent record table

for (i in 1:length(species)){
  for (j in 1:J){
    sub<- recs[recs$Camr_Nm==stations[j] & recs$Species == species[i],]
    sub<- na.omit(sub)
    if(dim(sub)[1]==0) next
    
    #OlsonNames(tzdir = NULL)
    ordsub<-order(as.POSIXlt(paste(sub$Date, sub$Time), tz = "Asia/Kuala_Lumpur", format= "%m/%d/%Y %H:%M"))
    sub2<- sub[ordsub,]
    
    dd<-unique(sub2$Date)
    
    dt<- as.POSIXlt(paste(sub2$Date, sub2$Time), tz = "Asia/Kuala_Lumpur", format= "%m/%d/%Y %H:%M")
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
recs.all<-recs.all[-1,]
recs.all
dim(recs.all) #457 independent records!
head(recs.all)
table(recs.all$Species)

#need to filter s (station list) to stations with occurences
filter <- unique(recs.all$Camr_Nm)

#start dataframe
tw <- s[1, ]

for(i in 1:159){
  for(r in 1:length(filter)){
    if(s$Camr_Nm[i] == filter[r]){
      tw <-rbind(tw, s[i, ])
    }
  }
}

#remove starter row
tw<-tw[-1,]

#rename cameraname in tw
tw$Cameraname <- tw$Camr_Nm

#since we are using all data, we do not have to filter by date (rename for convenience later)
recs_tw <- recs.all

active<- data.frame(CTname=NA, FirstDay=NA, LastDay=NA)
for(ct in 1:nrow(tw)){
  x<- recs_tw[recs_tw$Camr_Nm==tw$Cameraname[ct],]
  date1<-  as.Date(x[,"Date"], "%m/%d/%Y", na.rm=T)
  active[ct,"CTname"]<- tw$Cameraname[ct]
  active[ct,c("FirstDay", "LastDay")]<- paste(range(date1))
}
active

cbind(active, tw)

ext.date<- range(na.omit(as.Date(c(active[,"FirstDay"], active[,"LastDay"]), "%Y-%m-%d")))
out<- as.character(seq(as.Date(ext.date[1]),as.Date(ext.date[2]), by="days"))

ston<- data.frame(matrix(0, nrow = nrow(active), ncol = length(out)))
colnames(ston)<- as.character(seq(as.Date(ext.date[1]),as.Date(ext.date[2]), by="days"))
rownames(ston)<- active$CTname

for(ct in 1:nrow(active)){
  if(active[ct,"FirstDay"]== "NA" | active[ct,"LastDay"]== "NA") next
  x<- as.character(seq(as.Date(active[ct,"FirstDay"]),as.Date(active[ct,"LastDay"]), by="days"))
  for(d1 in 1: length(x)){
    for(d2 in 1:length(out)){	
      if(x[d1]==out[d2]){ston[ct, d2]<-1}  
    }
  }
}
head(ston)
str(ston)
dim(ston)
rowSums(ston)
range(rowSums(ston))

#days each camera active
rowSums(ston[,-1])
sum(rowSums(ston[,-1])) #3546 total active days
sum(rowSums(ston[,-1]))/159 #47.987 mean camera active days
sd(rowSums(ston[,-1])) #sd 35.915

colnames(recs.all)<- c( "station", "species", "date", "hour")
head(recs.all)

#write to csv
write.csv(recs.all, "recs.all_MalayanTapir.csv", row.names=F)
write.csv(ston, "ston_MalayanTapir.csv", row.names=T)


############################################################################################
### Creating Capture Histories using comm_hist_maker

recs1<- recs.all #change name for convenience
ston <- read.csv("ston_MalayanTapir.csv")
head(recs1)
dim(recs1)
head(ston)
#ston<- ston[,-1]
rowSums(ston[,-1])

unique(recs1$station)
recs<- recs1[1,]
for(ct in 1: nrow(ston)){
  for(r in 1:nrow(recs1)){
    if(recs1[r,"station"] == tw$Cameraname[ct]){recs[r,]<- recs1[r,]} else{}
  }
}

#recs<- recs[-1,]
#nas<- which(is.na(recs$station))
#recs<-recs[-nas,]
dim(recs)
dim(recs1)
ston$Estacao <- ston$X
sort(unique(recs$station)) == sort(unique(ston$Estacao))


records= recs; stations_on = ston; n.collap.days= 7
#works to here!


###=========================================================================================================###
###        	Function to create independent records table, collapsed capture histories and 
###          collapsed record histories
###=========================================================================================================###

comm_hist_maker<-function(records, stations_on, n.collap.days){
  
  ## make sure, station names are always in upper case in both files
  stations_on$Estacao<-toupper(stations_on$Estacao)
  records$station<-toupper(records$station)
  
  ##get first and last date in camera functioning matrix (dates start with "X")
  xx<-colnames(stations_on)[min(grep("20", colnames(stations_on) ))]
  first<-as.Date(substr(xx, 2, nchar(xx)), "%Y.%m.%d")
  xy<-colnames(stations_on)[max(grep("20", colnames(stations_on) ))]
  last<-as.Date(substr(xy, 2, nchar(xy)), "%Y.%m.%d")
  
  ##extract only 0/1 info from station_on
  stations_on.bin<-stations_on[,min(grep("20", colnames(stations_on) )):max(grep("20", colnames(stations_on) ))]
  
  ##based on days to collapse, get start and end date of each occasion
  ndays<-as.numeric(last-first)+1  #number of days
  if(ndays != dim(stations_on.bin)[2]) stop("Something went wrong selection columns")
  
  K<- ceiling(ndays/n.collap.days) #number of occasions
  
  beg<-seq(first, last, by=n.collap.days)
  en<-seq(first+(n.collap.days-1), last, by=n.collap.days)
  if(length(en)<K) {en<-c(en, last)} #add end day for last occasion if it's incomplete
  
  
  J<-dim(stations_on)[1] #number of sites
  
  ##calculate collapsed effort
  eff.coll<-matrix(0, J, K)
  
  #create occasion index
  occ.l<-(en-beg)+1
  occ<-NULL
  for (k in 1:K){
    occ<-c(occ,rep(k,occ.l[k] ))
  }
  
  for (j in 1:J){
    for (k in 1:K){
      eff.coll[j,k]<-sum(stations_on.bin[j, which(occ==k)])
    }
  }
  
  
  ### get independent records table #####################################################
  
  #extract only necessary columns, remove records with NA as date
  recs<-records[!is.na(records$date),c("station", "species", "date", "hour")]
  if (sum(is.na(pmatch(unique(recs$station), stations_on$Estacao, duplicates.ok = TRUE))) >0) stop("station mismatch")
  
  stations<-stations_on$Estacao
  species<-sort(unique(recs$species))
  
  recs.all<-recs[1,] #set up structure of independent record table
  
  for (i in 1:length(species)){
    
    for (j in 1:J){
      sub2<-recs[recs$station==stations[j] & recs$species == species[i] ,]
      if(dim(sub2)[1]==0) next
      
      dd<-unique(sub2$date)
      
      #dt<-strptime( paste(sub2$date, sub2$hour), "%m/%d/%Y %H:%M:%S")
      dt<- as.POSIXlt(paste(sub2$date, sub2$hour), tz = "America/Costa_Rica", format= "%m/%d/%Y %H:%M:%S")
      
      
      nr<-length(dt)
      
      recs.all<-rbind(recs.all, sub2[1,]) #always add first record
      
      
      if(dim(sub2)[1]>1) { #if more than one, check for independence
        
        for (k in 2:nr){
          dif<-difftime(dt[k], dt[k-1], units="mins")
          if(dif>60) {recs.all<-rbind(recs.all,sub2[k,] )}
        }
      }
    }
  }
  
  #remove starter row
  recs.all<-recs.all[-1,]
  
  
  #### make collapsed capture history ###################################################
  
  record.mat<-array(0, c(J, K,length(species)))
  
  for (k in 1:K){
    
    dl<-seq.Date(beg[k], en[k], by=1)
    
    for (i in 1:length(species)){
      for (j in 1:J){
        subb<-recs.all[recs.all$station == stations[j] &
                         recs.all$species==species[i] &
                         as.Date(recs.all$date, "%d/%m/%Y") %in% dl,]
        if(dim(subb)[1]==0) next
        record.mat[j,k,i]<-dim(subb)[1]
      }
    }
  }
  
  ## set to NA where effort == 0
  record.mat[eff.coll==0]<-NA
  
  ## turn number of records into binary
  record.mat2<-record.mat
  record.mat2[record.mat>0]<-1
  
  dimnames(record.mat2)[[3]]<-dimnames(record.mat)[[3]]<-species
  dimnames(record.mat2)[[1]]<-dimnames(record.mat)[[1]]<-stations
  
  f.visit<-apply(eff.coll, 1, function(x){min(which(x>0))})
  l.visit<-apply(eff.coll, 1, function(x){max(which(x>0))})
  k2<- ceiling(max(rowSums(eff.coll))/n.collap.days)+1
  crop_rec.mat<- array(NA,dim = c(length(stations), k2, length(species)))
  crop_rec.mat2<- array(NA,dim = c(length(stations), k2, length(species))) 
  crop_eff.coll<- matrix(NA, length(stations), k2)
  for (j in 1:length(stations)){ 
    r<- (l.visit[j]-f.visit[j])+1
    crop_rec.mat[j,1:r,]<- record.mat[j,f.visit[j]:l.visit[j],]
    crop_rec.mat2[j,1:r,]<- record.mat2[j,f.visit[j]:l.visit[j],]
    crop_eff.coll[j,1:r]<- eff.coll[j,f.visit[j]:l.visit[j]]  
  }
  
  dimnames(crop_rec.mat)[[3]]<-dimnames(crop_rec.mat2)[[3]]<-species
  dimnames(crop_rec.mat)[[1]]<-dimnames(crop_rec.mat2)[[1]]<- rownames(crop_eff.coll)<-stations
  
  
  return(list("Collapsed record history"=crop_rec.mat, 
              "Collapsed capture history"=crop_rec.mat2, 
              "Occasion effort"=crop_eff.coll,
              "Independent detections"=recs.all))
}
#---------------------------------------------------------------------------------------------###

