** CR_Master history. Christian Anderson, July 7, 2023.

The master csv from may 2022 was updated with 2021 march-feb(2022) kamuk survey data. There were some issues adding a new dataframe if 
the date field was stored as a "Date" class object in R, which was necessary to derive the season field. The new df's "Date" class forced the destination to the 
same type, leading to lossy dates, i.e., 02/15/2014 was converted to 02/15/20. I fixed the issue by redoing the 
extraction and additions, using the two Kamuk dataframes we had created on WildID (March-July & July-Feb), converting the date back to a 
character class before joining it to the master. 

Secondly, I forgot to make sure that animals sighted within 30 min of each other at the same camera are each independent records. I fixed that with the fourth *and* 
statement in the function. 

Using the function below, the independent field was derived for independence at 30 minutes (fixAgnosticIndependence.R). I also converted the "Yes" or "yes" and "No" or "no" to 1 or 0 to be more consistent.
```R
test_independence <- function(dat) { 
  
  #create datetime field for time math
  dat$datetime<- as.POSIXct(paste(base::as.Date(dat$Date, format = "%m/%d/%Y") , dat$Time),
                        tz = "America/Costa_Rica",
                        format = "%Y-%m-%d %H:%M:%S")
  
  #order data by datetime ascending (old-new) so that each record follows the previous chronologically
  dat<- dat[order(dat$datetime),]
  
  # Find the duplicate records based on latitude, longitude, and time difference and whether animal is different, 
  # (two records <30min apart of diff animals should be independent)
  duplicate_indices <-  dat$Latitude[-1] == dat$Latitude[-nrow(dat)] &
    
                        dat$Longitude[-1] == dat$Longitude[-nrow(dat)]&
    
                        abs(difftime(dat$datetime[-1], 
                                      dat$datetime[-nrow(dat)], 
                            units = "mins")) <= 30 &
    
                        dat$Common[-1] == dat$Common[-nrow(dat)]
  
  # set independence based on the duplicate indices 
  dat$newIndependent <- 1 #default to 1
  dat$newIndependent[duplicate_indices] <- 0 #if duplicate then 0
  
  
  # Return the modified data frame
  return(dat) #return dataframe with new independence  column
}
```
This function takes the location info and offsets each field as a vector, one index off the beginning and end, respectively, and compares those together 
so that each subsequent index is compared to the previous. This reduced the cost of looping over the (350,000, 29) dataset. 

**** Issues
It appears that some surveys have several records with incorrect dates, like Chirripo 2016 which has several dates during 2010. The newest 
version, Master(2023-7-10).csv, is missing the datetime and X.1 columns, but they are not important anyways. 

The Costa Rica study site start dates are
* Savegre Valley (2010)
* Los Quetzales NP (2011)
* Tapantí NP (2012)
* CB Alexander Skutch (2012)
* Chirripó NP (2012)
* Refugio La Marta ( 2013)
* El Copal Reserve (2013)
* La Cangreja NP (2014)
* Carara NP (2014)
* Las Lapas BC (2014)
* Villa Mills (2014)
* Cabo Blanco NP (2015)
* Osa Campanario (2016)
* BC Bosque de Agua (2016)
* PILA Valle de Silencio (2017)
* PILA Cerro Kamuk (2018)
