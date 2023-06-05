rm(list = ls())
data<- read.csv("all Tapir's data/Peru (Mountain Tapir)/Model Selection/Mt_T_Covs4.csv")

col.dim<- dim(data)[2]



library('corrgram')

corrgram(data[, 3:col.dim], 
         order=TRUE,
         main="Mountain Tapir covariates",
         lower.panel=panel.shade,
         upper.panel=panel.pie,
         diag.panel=panel.minmax, 
         text.panel=panel.txt)
