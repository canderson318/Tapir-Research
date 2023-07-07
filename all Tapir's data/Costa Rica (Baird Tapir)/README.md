** CR_Master history. Christian Anderson, July 7, 2023.

The master csv from may 2022 was updated with 2021 march-feb(2022) kamuk survey data. There were some issues adding a new dataframe if 
the date field was stored as a "Date" class object in R, which was necessary to derive the season field. I fixed the issue by redoing the 
extraction and additions, using the two Kamuk dataframes we had created on WildID (March-July & July-Feb), converting the date back to a 
character class before joining it to the master. 

Using the function below, the independent field was derived for independence at 30 minutes. 
```R
	test_independence <- function(data) { 
	  
	  # Find the duplicate records based on latitude, longitude, and time difference and current independent value
	  duplicate_indices <- data$Latitude[-1] == data$Latitude[-nrow(data)] &
		data$Longitude[-1] == data$Longitude[-nrow(data)] &
		abs(difftime(data$datetime[-1], data$datetime[-nrow(data)], units = "hours")) <= 0.5
	  # Create a new column 'newIndependent' and set its values based on the duplicate indices
	  data$newIndependent <- 1
	  data$newIndependent[duplicate_indices] <- 0
	  
	  # Return the modified data frame
	  return(data) #return dataframe with independence column filled
	}
```
This function takes the location info and offsets each field as a vector twice, by one index on either end, and compares those together 
so that each subsequent index is compared to the previous. 

**** Issues
It appears that some surveys have several records with incorrect dates, like Chirripo 2016 which has several dates during 2010. The newest 
version, Master(2023-7-7)-2.csv, is missing the datetime and X.1 columns, but they are not important anyways. 

The Costa Rica study site start dates include
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
