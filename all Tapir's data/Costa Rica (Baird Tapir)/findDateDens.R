rm(list = ls())
library(lubridate)

CR<- read.csv("C:/Users/chris/Documents/Research/Tapir-Research/all Tapir's data/Costa Rica (Baird Tapir)/CR_Master(2023-7-10).csv")

df<- CR

df$datetime<- base::as.Date(df$datetime,  format ="%Y-%m-%d %H:%M:%S")
diff<- diff.Date(na.omit(df$datetime), units = "weeks")

boxplot(scale(df$datetime), breaks = c("2010-01-01", "2022-01-01"))

hist(df$datetime, breaks = c(min(df$datetime), ))

# k= 0
# cams<- c()
# for(i in 1:nrow(CR_eff)){
#   r_nam<- rownames(CR_eff)[i]
#   for(j in 1:ncol(CR_eff)){
#     if(!is.na(CR_eff[i,j])){
#       k= k+CR_eff[i,j]
#     }
#     cams<- c(cams, r_nam = k)
#     k=0
#   }
# }
# cams
# length(cams)
