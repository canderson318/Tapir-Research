###=================================================================================###
### Script to clean Malayan Tapir Data and produce Community Detection Histories   
### Honor's Project - 2023	        							                                	###
### 19 March 2023 											                                            ###
### Sarah Turcic        										                                        ###
###=================================================================================###

#clear system
rm(list=ls())

#set wd
setwd("C:/Sarah/Point_Loma/Courses/7. Fall 2022 (complete)/Honor's Project/Tapirs!/Malayan Data/Raw Data/Rayan_Shariff")
dir()

#read in data
dat<- read.csv("TapirDataMalaysia_2009-2011.xlsx - Data.csv")
#info <- read.csv("Malaysia_Sampling_Dates.csv")
s <- read.csv("Malaysia_Selected_Run_Times.csv")
head(dat)
dim(dat)

#length(unique(info$Camr_Nm)) #329 cameras
length(unique(dat$Camr_Nm)) #159 cameras with detection
length(unique(s$Camr_Nm)) #329 cameras - all here!

#filter for duplicated sampling dates
#duplicated <- info[duplicated(info$Camr_Nm),]

#hecka for statement to get cameras with no detection to sort out cams w/ no detect
#need to filter s (station list) to stations with occurences
filter_rec <- unique(dat$Camr_Nm) #159
#filter <- unique(info$Camr_Nm) #329

#load tidyverse
library(tidyverse)

#filter based on values not in dat
#no_recs <- filter(info, !(Camr_Nm %in% dat$Camr_Nm)) #220 some repeated

#filter based on values in dat
#yes_recs <- filter(info, Camr_Nm %in% dat$Camr_Nm) #228 some repeated

#How many different start and end dates
length(unique(paste(dat$Camr_Nm, dat$Cmr_S_D, dat$Cmr_E_D))) #228 (some are repeated for multiple start and end dates) - all start and end dates accounted for

#combine datasets to account for all running cameras (even those with no records)
recs <- dat[, c("Camr_Nm", "Species", "Common", "Cmr_S_D", "Cmr_E_D", "Date", "Time")]
#write.csv(no_recs, "no_recs_formatted.csv") #wrote to csv to manually fill out NAs
recs_add <- read.csv("no_recs_formatted_complete.csv")
recs_total <- rbind(recs, recs_add) #now total 1029 observations - all accounted for


#############################################################################
### get independent records table ###########################################

#extract only necessary columns, remove records with NA as date
recs<-dat[!is.na(dat$Date),c("Camr_Nm", "Species", "Date", "Time")] #809 observations - all accounted
dim(recs)
head(recs)

#rename camera name to ctid for convenience
colnames(recs)[1] <- "ctid"

#create stations list
stations<-unique(recs$ctid) #159 stations w/ recs
stations
J<- length(stations)
species<-sort(unique(recs$Species)) #1 species (good!)


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
recs.all <- recs.all[-1,] #457 independent records
dim(recs.all)
head(recs.all)
table(recs.all$Species)

###works to here!

#################################################################################################
#filter recs.all for dates selected

tw<- read.csv("Malayan_Selected_Filter_Dates.csv")

#filter based on values in dat
tw <- filter(tw, Camr_Nm %in% dat$Camr_Nm) #159! 
sort(tw$Camr_Nm)==sort(unique(recs.all$ctid)) #True!
head(recs.all)
colnames(recs.all)

#change ctid back to Camr_Nm
colnames(recs.all)[1] <- "Camr_Nm"

recs_tw<- data.frame()
for(ct in 1:nrow(tw)){
  x<- recs.all[recs.all$Camr_Nm==tw$Camr_Nm[ct],]
  date1<-  as.Date(x[,"Date"], "%m/%d/%Y", na.rm=T)
  for(r in 1:nrow(x)){
    if(tw[ct,"StartYear"] == tw[ct,"EndYear"]){
      if(as.numeric(format(date1[r], "%Y"))== tw[ct,"StartYear"] &
         as.numeric(format(date1[r], "%m"))>= tw[ct,"StartMonth"] &
         as.numeric(format(date1[r], "%m"))<= tw[ct,"EndMonth"])
      {recs_tw<- rbind(recs_tw, x[r,])}}
    if(tw[ct,"StartYear"] != tw[ct,"EndYear"]){
      if(as.numeric(format(date1[r], "%Y"))== tw[ct,"EndYear"] &
         as.numeric(format(date1[r], "%m"))<= tw[ct,"EndMonth"] |
         as.numeric(format(date1[r], "%Y"))== tw[ct,"EndYear"]-1 &
         as.numeric(format(date1[r], "%m"))>= tw[ct,"StartMonth"])
      {recs_tw<- rbind(recs_tw, x[r,])}}
  }
}

dim(recs_tw) #398 records
length(unique(recs_tw$Camr_Nm)) #153 some cameras dropped with same start and end month


#######################################################################################################

#since we are using all data (based on dates pre-selected), we do not have to filter by date (rename for convenience later)
recs_loc_tw <- recs.all
colnames(s)[2] <- "Cmr_S_D"
colnames(s)[3] <- "Cmr_E_D"

#convert start and end date to dates in s table
s$Cmr_S_D <- as.Date(s$Cmr_S_D, "%m/%d/%Y")
s$Cmr_E_D <- as.Date(s$Cmr_E_D, "%m/%d/%Y")
head(s)
active <- s[,c("Camr_Nm","Cmr_S_D","Cmr_E_D")]

#cbind(active, tw)

ext.date<- range(na.omit(as.Date(c(active[,"Cmr_S_D"], active[,"Cmr_E_D"]), "%Y-%m-%d")))
out<- as.character(seq(as.Date(ext.date[1]),as.Date(ext.date[2]), by="days"))

#input all cameras here
ston<- data.frame(matrix(0, nrow = nrow(active), ncol = length(out)))
colnames(ston)<- as.character(seq(as.Date(ext.date[1]),as.Date(ext.date[2]), by="days"))
rownames(ston)<- active$CTname

for(ct in 1:nrow(active)){
  #if(active[ct,"Cmr_S_D"]== "NA" | active[ct,"Cmr_E_D"]== "NA") next 
  x<- as.character(seq(as.Date(active[ct,"Cmr_S_D"]),as.Date(active[ct,"Cmr_E_D"]), by="days"))
  for(d1 in 1: length(x)){
    for(d2 in 1:length(out)){	
      if(x[d1]==out[d2]){ston[ct, d2]<-1}  
    }
  }
}

#add rownames
rownames(ston) <- s$Camr_Nm

head(ston) #everything worked, table just huge so you really have to scroll to see 1s!
str(ston)
dim(ston)

#days each camera active
rowSums(ston)
sum(rowSums(ston)) 	#28,792 total active days
mean(rowSums(ston))	#87.51 mean camera active days
sd(rowSums(ston)) 	#sd mean camera active days 27.734 

#write to csv
#write.csv(ston, "ston_Malayan_Tapir.csv", row.names=T)


############################################################################################
### Creating Capture Histories using comm_hist_maker

recs1<- recs_tw #change name for convenience
colnames(recs1) <- c("station", "species", "date", "hour")
ston <- read.csv("ston_Malayan_Tapir.csv") #read in to deal with first column being unamed
head(recs1)
dim(recs1)
head(ston)
rowSums(ston[,-1])

# Just making sure all detections are from valid cts 
unique(recs1$station)
recs<- recs1[1,]
for(ct in 1: nrow(ston)){
  for(r in 1:nrow(recs1)){
    if(recs1[r,"station"] == s$Camr_Nm[ct]){recs[r,]<- recs1[r,]} else{}
  }
}

recs<- recs[-1,]
#nas<- which(is.na(recs$station))
#recs<-recs[-nas,]
dim(recs)
dim(recs1)
colnames(ston)[1]<- "Estacao"
head(ston)
sort(unique(recs$station)) == sort(unique(ston$Estacao)) #all false, but doesn't seem to be a problem?


records= recs; stations_on = ston; n.collap.days= 7
#works to here!

head(recs)
unique(recs$station)
recs_cts<- as.data.frame(table(recs$station))
recs_cts<- recs_cts[order(recs_cts$Var1),]

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
      dt<- as.POSIXlt(paste(sub2$date, sub2$hour), tz = "Asia/Singapore", format= "%m/%d/%Y %H:%M")
      
      
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
                         recs.all$species == species[i] &
                         as.Date(recs.all$date, "%m/%d/%Y") %in% dl,]		# THERE WAS A PROBLEM HERE, DAYS AND MONTHS WERE SWAPED
        if(dim(subb)[1]==0) next
        record.mat[j,k,i]<-dim(subb)[1]
      }
    }
  }
  
  #sum(record.mat)
  #length(which(rowSums(record.mat)>0))
  
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

###----------------------------------------------------------------------------------###
###                     Running basic occupancy models	            								 ###
###----------------------------------------------------------------------------------###

malayan_tapir <- comm_hist_maker(records= recs, stations_on = ston, n.collap.days= 7)
str(malayan_tapir)

sum(malayan_tapir[[2]][,,"Tapirus indicus"],na.rm=T) #339 records

#separate by species (separate out blanks) and export as rds 
#collapsed records
#independent <- nrow(mountain_tapir[[4]])
#saveRDS(independent, "Collapsed_Records_Mountain_Tapir_DR.rds")
#saveRDS(mountain_tapir, "Mountain_Tapir_DR.rds")

#effort
eff<-malayan_tapir[[3]]
sum(rowSums(eff, na.rm = TRUE)) #28792! effort
#saveRDS(eff, "Effort_Malayan_Tapir.rds")

#collapsed capture history
Ma_capture <- malayan_tapir[[2]][,,"Tapirus indicus"] #must be a matrix
#saveRDS(Ma_capture, "Collapsed_Capture_Malayan_Tapir.rds")

#check how many cameras with capture
count <- rowSums(Ma_capture, na.rm=TRUE)
#count <- as.data.frame(count) 					#only 10 cameras with capture?
ct_count <- length(which(rowSums(Ma_capture,na.rm=T)>0))		#150 with capture!

library(unmarked)
umf_ma<- unmarkedFrameOccu(y=Ma_capture, siteCovs=, obsCovs=list(Eff=eff))
summary(umf_ma) #150 sites with detection! Max 19 observations per site

# Running Null model

mod0 <- occu(~1~1, umf_ma)  # Null Model
summary(mod0)
plogis(0.188)  	# Probability of occupancy 0.5468
plogis(-1.82)	# Probability of detection 0.139

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf_ma)  # Null Model
summary(m.psi1.pEff) 	# THIS MODEL MAKES SENSE NOW. IT HAS A LOWER AIC THAN THE NULL MODEL AND EFFORT HAS A POSITIVE EFFECT ON DETECTION
plogis(0.194) #prob of occupancy 0.5483
plogis(0.339) #prob of detection 0.5829 - effort influences detection!


###################################################################################################################
#All coordinates###

#import all coordinates table
all_coords <- read.csv("")

