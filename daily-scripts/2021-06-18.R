rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)




#This section fits the first model y=mx to the data sets. 
#This first bit of code was lifted from Spencer's prep work. 
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

m <- sum(y*x/sum(x^2))
f <- function(x,m){m*x}

xM <- seq(min(x),max(x),0.01)
yM <- f(xM,m=m)
plot(xM,yM,type = "l")

par(mfrow=c(1,1), mar = c(2,2,0.5, 0.5))
plot(x,y,pch=16)
lines(xM,yM,type="l",col="green")


#We could just put this in a function

fit1 <- function(data){
  x <- data$x
  y <- data$y
  plot(x,y)
  
  m <- sum(y*x/sum(x^2))
  f <- function(x,m){m*x}
  
  xM <- seq(min(x),max(x),0.01)
  yM <- f(xM,m=m)
  #plot(xM,yM,type = "l")
  
  par(mfrow=c(1,1), mar = c(2,2,0.5, 0.5))
  plot(x,y,pch=16)
  lines(xM,yM,type="l",col="green")
}
fit1(read.csv(url("https://byuistats.github.io/M119/data1_ls.csv")))
fit1(read.csv(url("https://byuistats.github.io/M119/data2_ls.csv")))
fit1(read.csv(url("https://byuistats.github.io/M119/data3_ls.csv")))
fit1(read.csv(url("https://byuistats.github.io/M119/data4_ls.csv")))
fit1(read.csv(url("https://byuistats.github.io/M119/data5_ls.csv")))
 log(-3)

#Let's now fit the third model to the data sets, so fit y=a ln(x) to the data sets. 
#I grabbed the code from your Jamboards. 
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

a <- sum(log(x)*y)/sum(log(x)^2)
f <- function(x,a){a*log(x)}

xM <- seq(min(x),max(x),0.01)
yM <- f(xM,a=a)
#plot(xM,yM,type = "l")

par(mfrow=c(1,1), mar = c(2,2,0.5, 0.5))
plot(x,y,pch=16)
lines(xM,yM,type="l",col="green")

#Now let's put it in a function
fit3 <- function(data){
  x <- data$x
  y <- data$y
  plot(x,y)
  
  a <- sum(log(x)*y,na.rm = TRUE)/sum(log(x)^2,na.rm = TRUE)
  f <- function(x,a){a*log(x)}
  
  xM <- seq(min(x),max(x),0.01)
  yM <- f(xM,a=a)
  #plot(xM,yM,type = "l")
  
  par(mfrow=c(1,1), mar = c(2,2,0.5, 0.5))
  plot(x,y,pch=16)
  lines(xM,yM,type="l",col="green")
}
fit3(read.csv(url("https://byuistats.github.io/M119/data1_ls.csv")))
fit3(read.csv(url("https://byuistats.github.io/M119/data2_ls.csv")))
fit3(read.csv(url("https://byuistats.github.io/M119/data3_ls.csv")))
fit3(read.csv(url("https://byuistats.github.io/M119/data4_ls.csv")))
fit3(read.csv(url("https://byuistats.github.io/M119/data5_ls.csv")))








#Let's now fit the fourth model to the data sets, so fit y=a exp(x) to the data sets. 
#I grabbed the code from Travis's work.  
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

a <- sum(2*y*exp(x))/sum(2*exp(2*x))
f <- function(x,a){a*exp(x)}

xM <- seq(min(x),max(x),0.01)
yM <- f(xM,a=a)
#plot(xM,yM,type = "l")

par(mfrow=c(1,1), mar = c(2,2,0.5, 0.5))
plot(x,y,pch=16)
lines(xM,yM,type="l",col="green")

#Now let's put it in a function
fit4 <- function(data){
  x <- data$x
  y <- data$y
  plot(x,y)
  
  a <- sum(2*y*exp(x))/sum(2*exp(2*x))
  f <- function(x,a){a*exp(x)}
  
  xM <- seq(min(x),max(x),0.01)
  yM <- f(xM,a=a)
  #plot(xM,yM,type = "l")
  
  par(mfrow=c(1,1), mar = c(2,2,0.5, 0.5))
  plot(x,y,pch=16)
  lines(xM,yM,type="l",col="green")
}
fit4(read.csv(url("https://byuistats.github.io/M119/data1_ls.csv")))
fit4(read.csv(url("https://byuistats.github.io/M119/data2_ls.csv")))
fit4(read.csv(url("https://byuistats.github.io/M119/data3_ls.csv")))
fit4(read.csv(url("https://byuistats.github.io/M119/data4_ls.csv")))
fit4(read.csv(url("https://byuistats.github.io/M119/data5_ls.csv")))










