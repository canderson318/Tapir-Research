rm(list = ls())
data<- read.csv("all Tapir's data/Costa Rica (Baird_s Tapir)/cv_t3.csv")
library(corrgram)
library(ggplot2)
library(gridExtra)
library(patchwork)

#layout(matrix(c(1, 2), nrow = 1))
par(mfrow = c(1, 2))

spearman<- corrgram(data[,3:14], 
                     order=TRUE,
                     cor.method = 'spearman',
                     main="Baird's Tapir covariates (Spearman's)",
                     lower.panel=panel.shade,
                     upper.panel=panel.pie,
                     diag.panel=panel.minmax, 
                     text.panel=panel.txt)
pearson<- corrgram(data[,3:14], 
                     order=TRUE,
                     cor.method = 'pearson',
                     main="Baird's Tapir covariates (Pearson's)",
                     lower.panel=panel.shade,
                     upper.panel=panel.pie,
                     diag.panel=panel.minmax, 
                     text.panel=panel.txt)

arranged<- spearman + pearson

arranged