rm(list = ls())

x<- runif(10000, 0, 1000)
xnorm<- rnorm(10000, 50, 20)

y<- log(x)
ynorm<- log(xnorm)

plot(xnorm, ynorm)
