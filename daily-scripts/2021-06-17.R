rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y
par(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
plot(x,y,pch=16)

#Let's compute b using the formula we develop on our jamboard. 
b <- (sum(y)*sum(x^2)-sum(x*y)*sum(x))/(sum(1+0*x)*sum(x^2)-sum(x)*sum(x))
b
#We found the coefficient for b that matches with googlesheets. 

#Here is the computation for m. 
m <- (sum(1+0*x)*sum(x*y)-sum(x)*sum(y))/(sum(1+0*x)*sum(x^2)-sum(x)*sum(x))
m

#Now lets verify that we actually found the parameters that minimize the sum of the squared errors .
fbb <- 120
fmm <- 2*sum(x^2)
fbm <- sum(2*x)

D <- fbb *fmm - fbm^2
D
fbb
#Since D>0 and fbb>0, we found a minimum. 

#We now plot the data along with the model.
f <- function(x,b,m){b+m*x}
#We need values for x and y to plot our model (not the data)
xM <- seq(-2,4,0.01)
yM <- f(xM, b=b, m=m)
plot(xM,yM,type = "l")

#Now let's plot the data with the model. 
par(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
plot(x,y,pch=16)
lines(xM,yM,type = "l",col = "green")


#This chunk of code relates to the fitting the 3rd data set to the 5th function. 
a <- sum(y*exp(-x))/sum(exp(-2*x))
f <- function(x,a){a*exp(-x)}

#We need values for x and y to plot our model (not the data)
xM <- seq(-2,4,0.01)
yM <- f(xM, a=a)
plot(xM,yM,type = "l")

#Now let's plot the data with the model. 
par(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
plot(x,y,pch=16)
lines(xM,yM,type = "l",col = "green")

