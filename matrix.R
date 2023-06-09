rm(list= ls())
x<- 1:5
y<- 1:5
mat<- outer(x,y, "*")
rownames(mat)<- x
colnames(mat)<- y
