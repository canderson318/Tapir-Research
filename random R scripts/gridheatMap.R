rm(list= ls())
library(ggplot2)
X<- sample(seq(1:20), 20, replace= TRUE)
Y<- sample(X, 20, replace = TRUE)
Z<- (Y+X)
df<- data.frame("x"=X,"y"=Y, "x+y" = Z)


ggplot(df, aes(x= x, y= y, fill= z))+
  geom_point(  size = 2.5)+
  scale_fill_gradient(
    low= "blue", high= "red",
    breaks= seq(70,170, by = 20),
    guide = guide_legend()
  
  )

mycolors <- rev(rainbow(4))
ggplot(df, aes(x=x, y=y))+
  geom_point(aes(color=x+y),  size=3)+
  #geom_smooth(method="lm")+ 
  scale_color_gradientn(colors=mycolors)

set.seed(123)                                                     # Set seed for reproducibility
data <- matrix(rnorm(100, 0, 10), nrow = 10, ncol = 10)           # Create example data
colnames(data) <- paste0("col", 1:10)                             # Column names
rownames(data) <- paste0("row", 1:10)    
heatmap(as.matrix(data))


