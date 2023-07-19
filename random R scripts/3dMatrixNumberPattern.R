
library(ggplot2)

f<- function(n){y= (0.5)*n^2 + (.5)*n ; return(y)}

b<- c(1,3,6,10,15,21,28,36,45,55)
n<- seq(1:10)
y<- f(n)
df<- as.data.frame(cbind(n,b,y))



ggplot(df, aes(x = d, y = b)) +
  geom_point(aes(x= n, y= y), shape = 2, color= 'red') +
  geom_point(aes(x= n, y= b), shape = 1, color = "blue")
  geom_smooth(aes(x= n, y= y), color= "red")+
  geom_smooth(aes(x= n, y= b), color = "blue")

lm(b ~ poly(n, 2, raw = TRUE))

  poly(n, 2, raw= TRUE)
  