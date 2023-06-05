# TapirResearch
## Tapir covariate analyses

**Analysis of camera trap data from four species of tapir. 

Created density plots from detection data for Baird’s, Mountain, Lowland, and Malayan Tapirs. All data was plotted using the densityPlot() function from the “overlap” package, using the average sunrise and sunset time for that data’s timeframe. (“temporalDensity” folder)

Collated temperature covariate data for the Malayan Tapir (maxMinTemps_MA.R) and added a month column (addDateColumn.R). Converted timestamps to solar time and plotted density plots for circadian activity (“temporalDensity folder”). Solar time standardizes the time of day to a value (in radians) that relates the location—and therefore intensity—of the sun. For example, the sun’s location at 07:00 in January will be different than 07:00 in July. To adjust for the variability in the sun's path and the camera's location on the Earth, the suncalc package utilizes the study site's time zone, as well as the camera's longitude and latitude, in conjunction with the detection time for each sighting. The density figures are plottted against each other to show overlapping activity levels between species using the overlap package.
