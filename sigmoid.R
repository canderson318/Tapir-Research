library(ggplot2)
x<- sample(seq(1, 10, length.out= 2000), 100, replace = TRUE)
y<- sample(seq(1, 2000, length.out=2000), 100, replace = TRUE)

x<- sample()

f<- function(x){r<- 1/(1-exp(1)^x); r}

ggplot(NULL, aes(x,f(y)))+
geom_point()+
geom_smooth()

speed<- seq(10,200, length.out= 20)
weight<- seq(5, 1, length.out= 20)
lm<- lm(speed~weight)
df<- data.frame(speed= speed, wgt= weight)

new.speed<- data.frame(100, 20, 50)
predict(lm, new.speed)
