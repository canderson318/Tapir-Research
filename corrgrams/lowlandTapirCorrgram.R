rm(list = ls())
data<- read.csv("all Tapir's data/Amazon (Lowland Tapir)/cv_t_AM_v2.csv")
library('corrgram')

corrgram(data[,3:14], order=TRUE,
         main="Lowland Tapir covariates",
         lower.panel=panel.shade,
         upper.panel=panel.pie,
         diag.panel=panel.minmax, 
         text.panel=panel.txt)