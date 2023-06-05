rm(list = ls())
data<- read.csv("all Tapir's data/Amazon (Lowland Tapir)/cv_t_AM_v2.csv")
library(ggcorrplot)
colnams <- colnames(data)

correlation_matrix <- cor(data[,3:14], 
                          method = "pearson")

ggcorrplot(correlation_matrix)

