rm(list = ls())
require(ggplot2)

f<- function(B0, B1, B2, X1, X2){return(B0 + B1*X1 + B2*X2)}

X1 <- seq(0, 10, length.out = 100)
X2 <- seq(0, 10, length.out = 100)

B0 <- 1
B1 <- 2
B2 <- 3
y <- f(B0, B1, B2, X1, X2)
y2<- f(B0)
ggplot(as.data.frame(cbind(X1,X2)))+
  geom_smooth(aes(x= X1, y= y), color = "blue")+
  geom_smooth(aes(x= X2, y=y), color = "red")
