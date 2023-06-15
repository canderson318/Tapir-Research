rm(list= ls())
library(ggplot2)

x<- sample(seq(1, 15, length.out= 200), 100, replace = TRUE)
f<- function(x){r<- x+x^2; r}

ggplot(NULL, aes(x= scale(x)))+
  geom_smooth(aes(y= f(x)), color= "red")+
  geom_smooth(aes(y= x), color = "green")
speed<- seq(10,200, length.out= 20)
weight<- seq(5, 1, length.out= 20)
lm<- lm(speed~weight)
df<- data.frame(speed= speed, wgt= weight)

new.speed<- data.frame(100, 20, 50)
predict(lm, new.speed)
