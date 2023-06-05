###=================================================================================###
### Script to clean Mountain Tapir Data and produce Community Detection Histories   
### Honor's Project - 2023	        							                                	###
### 12 March 2023 											                                            ###
### Sarah Turcic        										                                        ###
###=================================================================================###

#clear system
rm(list=ls())

#set wd
setwd("C:/Sarah/Point_Loma/Courses/7. Fall 2022 (complete)/Honor's Project/Tapirs!/Peru Data - Mountain Tapir/Raw Data")
dir()

#read in data
mt_dat<- read.csv("all_data_Mountain4.csv" )
head(mt_dat)
dim(mt_dat)
length(sort(unique(mt_dat$Camr_Nm)))	# Camr_Nm GOES FROM 1 TO 87, THERE IS NO CT 33 AND NO 44!

s <- read.csv("Data Formatting - Global Tapir Project_JLM_stations.csv")
head(s)
tail(s)
dim(s)	
length(sort(unique(mt_dat$Camr_Nm)))	# OK, IT MATCHES WITH mt_dat!

#fix error in date format
s$Cmr_E_D[34] <- "08/29/2016"
mt_dat$Cmr_E_D[57] <- "08/29/2016"
mt_dat$Cmr_E_D[58] <- "08/29/2016"

#unique stations - 85 (28 with records)
stat<- sort(unique(mt_dat$Camr_Nm))
stat

#explore data
mt_dat$Date
table(mt_dat$Species)
colnames(mt_dat)
dim(mt_dat)

#create unique survey name
unique(paste(mt_dat$Srvy_Nm, mt_dat$Camr_Nm))
unique(paste(mt_dat$Site, mt_dat$Camr_Nm))
dat1<- cbind(mt_dat,paste(mt_dat$Site, mt_dat$Camr_Nm))
head(dat1)

#change column names
colnames(dat1)[6]<- "Camr_Nm1"
colnames(dat1)[17]<- "Camr_Nm"
unique(dat1$Camr_Nm) #there are 28 unique survey names, MEANING THAT TAPIRS WERE DETECTED IN 28 DIFFERENT CTS

#combine station names for stations
s<- cbind(s,paste(s$Site, s$Camr_Nm))
head(s)
#change column names
colnames(s)[6]<- "Camr_Nm1"
colnames(s)[15]<- "Camr_Nm"

#extract only necessary columns, remove records with NA as date - no NAs in this dataset
#recs<-dat1[!is.na(dat1$Date),c("Camr_Nm", "Species", "Date", "Time")]
recs_loc <- dat1[!is.na(dat1$Date),c("Camr_Nm", "Species", "Date", "Time", "Latitud", "Longitd")]	# REMOVING ROW THAT ARE NOT TAPIR RECORDS, BUT WERE ADDED BY SARAH
dim(recs_loc)


##########################################################################################################
### Independent Records ############################
stations<-unique(recs_loc$Camr_Nm)
stations

J<- length(stations)
species<-sort(unique(recs_loc$Species))

recs_loc.all<-recs_loc[1,] #set up structure of independent record table

for (i in 1:length(species)){
  for (j in 1:J){
    sub<- recs_loc[recs_loc$Camr_Nm==stations[j] & recs_loc$Species == species[i],]
    sub<- na.omit(sub)
    if(dim(sub)[1]==0) next
    
    #OlsonNames(tzdir = NULL)
    ordsub<-order(as.POSIXlt(paste(sub$Date, sub$Time), tz = "America/Lima", format= "%m/%d/%Y %H:%M:%S"))
    sub2<- sub[ordsub,]
    
    dd<-unique(sub2$Date)
    
    dt<- as.POSIXlt(paste(sub2$Date, sub2$Time), tz = "America/Lima", format= "%m/%d/%Y %H:%M:%S")
    nr<-length(dt)
    
    recs_loc.all<-rbind(recs_loc.all, sub2[1,]) #always add first record
  
    if(dim(sub2)[1]>1) { #if more than one, check for independence
      
      for (k in 2:nr){
        dif<-difftime(dt[k], dt[k-1], units="mins")
        if(dif>(24*60)) {recs_loc.all<-rbind(recs_loc.all,sub2[k,] )}
      }
    }
  }
}

#remove starter row
recs_loc.all<-recs_loc.all[-1,]
recs_loc.all
dim(recs_loc.all) #95 independent mountain tapir records!
length(unique(recs_loc.all$Camr_Nm)) #28 cameras with records
head(recs_loc.all)

#rename recs_loc.all to tw based on prestructured code
tw <- recs_loc.all
tw$Cameraname <- tw$Camr_Nm

#since we are using all data, we do not have to filter by date (rename for convenience later)
recs_loc_tw <- recs_loc.all

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

head(ston)
str(ston)
dim(ston)

#days each camera active
rowSums(ston)
sum(rowSums(ston)) 	#9456 total active days
mean(rowSums(ston))	#111.2471 mean camera active days
sd(rowSums(ston)) 	#sd mean camera active days 30.91 

colnames(recs_loc.all)<- c( "station", "species", "date", "hour", "lat", "long")
head(recs_loc.all)

#write to csv
#write.csv(recs_loc.all, "recs_loc.all_MountainTapir_revised_DR.csv", row.names=F)
write.csv(ston, "ston_MoutainTapir_revised_DR.csv", row.names=T)


############################################################################################
### Creating Capture Histories using comm_hist_maker

recs1<- recs_loc.all #change name for convenience
ston <- read.csv("ston_MoutainTapir_revised_DR.csv") #read in to deal with first column being unamed
head(recs1)
dim(recs1)
head(ston)
#ston<- ston[,-1]
rowSums(ston[,-1])

# Just making sure all detections are from valid cts 
unique(recs1$station)
recs<- recs1[1,]
for(ct in 1: nrow(ston)){
  for(r in 1:nrow(recs1)){
    if(recs1[r,"station"] == s$Camr_Nm[ct]){recs[r,]<- recs1[r,]} else{}
  }
}

#recs<- recs[-1,]
#nas<- which(is.na(recs$station))
#recs<-recs[-nas,]
dim(recs)
dim(recs1)
colnames(ston)[1]<- "Estacao"
#ston$Estacao <- ston$X
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

mountain_tapir <- comm_hist_maker(records= recs, stations_on = ston, n.collap.days= 7)
str(mountain_tapir)

sum(mountain_tapir[[2]][,,"Tapirus pinchaque"],na.rm=T)

#separate by species (separate out blanks) and export as rds 
#collapsed records
#independent <- nrow(mountain_tapir[[4]])
#saveRDS(independent, "Collapsed_Records_Mountain_Tapir_DR.rds")
#saveRDS(mountain_tapir, "Mountain_Tapir_DR.rds")


#effort
eff<-mountain_tapir[[3]]
sum(rowSums(eff, na.rm = TRUE)) #9456! effort
#saveRDS(eff, "Effort_Mountain_Tapir_revised_DR.rds")
#saveRDS(eff, "Effort_Mountain_Tapir_DR.rds")

#collapsed capture history
mt_capture <- mountain_tapir[[2]][,,"Tapirus pinchaque"] #must be a matrix
#saveRDS(mt_capture, "Collapsed_Capture_Mountain_Tapir_revised_DR.rds")

#check how many cameras with capture
#count <- rowSums(mt_capture, na.rm=TRUE)
#count <- as.data.frame(count) 					#only 10 cameras with capture?
#ct_count <- length(which(rowSums(mt_capture,na.rm=T)>0))		#AFTER FIXING THE CODE< THERE ARE 28 CT WITH CAPTURE!

library(unmarked)
umf_mt<- unmarkedFrameOccu(y=mt_capture, siteCovs=, obsCovs=list(Eff=eff))
summary(umf_mt) #28 sites with detection! Max 22 observations per site

# Running Null model

mod0 <- occu(~1~1, umf_mt)  # Null Model
summary(mod0)
plogis(-0.595)  	# Probability of occupancy 0.35
plogis(-1.56)	# Probability of detection 0.17

# Running model with Eff as survey covariate
m.psi1.pEff<- occu(~Eff~1, umf_mt)  # Null Model
summary(m.psi1.pEff) 	# THIS MODEL MAKES SENSE NOW. IT HAS A LOWER AIC THAN THE NULL MODEL AND EFFORT HAS A POSITIVE EFFECT ON DETECTION

