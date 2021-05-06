library(data4led)
dist <- led_time(2100)
x <- dist$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
hist(x,probability = TRUE, main="Histogram of Lightbulb Intensities \n after 2104 hours")


f2 <- function(L,h=0,a=1,b=5){
  # Make sure a > 0 and b > 0.
  
  out <- rep(-1,length(L))
  out[(L < h)] <- 0*L[(L < h)]
  out[(L >= h)] <- b^a/gamma(a)*(L[(L >= h)]-h)^(a-1)*exp(-b*(L[(L >= h)]-h))
  
  return(out)
}

L1 <- seq(98,105,0.01)
y1 <- f2(L1,h=98.6,a=25.8,b=8.5)
x <- dist$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
hist(x,probability = TRUE,xlim=c(98,105),ylim=c(0,0.8),main="Histogram of Lightbulb Intensities \n with fitted f2 function")
lines(L1,y1,col=2)





f1 <- function(L,h=0,a=1){
  #Make sure h > 0 and a > 0.
  1/sqrt(2*pi*a)*exp(-(L-h)^2/(2*a))
}

L2 <- seq(98,105,0.01)
y2 <- f1(L2,h=101.5,a=0.35)
x <- dist$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
hist(x,probability = TRUE,xlim=c(98,105),ylim=c(0,0.8),main="Histogram of Lightbulb Intensities \n with fitted f1 function")
lines(L2,y2,col=2)







f0 <- function(L,a=0,b=1){
  # Make sure a < b.
  
  out <- rep(0,length(L))
  out[(L <= a)] <- 0
  out[(a < L) & (L < b)] <- 1/(b-a) + 0*L[(a < L) & (L < b)]
  out[(L >=b)] <-0
  
  return(out)
}

L3 <- seq(98,105,0.01)
y3 <- f0(L3,a=99.5,b=103.5)
x <- dist$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
hist(x,probability = TRUE,xlim=c(98,105),ylim=c(0,0.8),main="Histogram of Lightbulb Intensities \n with fitted f0 function")
lines(L3,y3,col=2)






library(data4led)
bulb <- led_bulb(1,seed = 2021)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)




f2 <- function(x,a0=0,a1=0,a2=1){
  a0 + a1*x + a2*x^2
}

x <- seq(-10,80001,2)
yM <- f2(x,a0=100,a1=0.00043,a2=-0.00000005)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)


f5 <- function(x,a0=100,a1=0,a2=1){
  (a0 + a1*x)*exp(-a2*x)
}

x <- seq(-10,80001,2)
yM <- f5(x,a0=100,a1=0.00487,a2=0.0000425)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)

rm(list=ls()) #Clears the environment

h <- function(x){
  3*x -15
}

#What does the $root code do below?
uniroot(h,c(2,6))$root

x <-seq(0,30,1)
plot(x,h(x), type="l")
abline(h=0, col = "lightgray")


h.shift <- function(x){
  h(x) - 4
}
x <-seq(0,30,1)
plot(x,h.shift(x), type="l")
abline(h=0, col = "lightgray")

uniroot(h.shift,c(-1000,99987))$root

uniroot(h.shift,c(0,10))$root


g <- function(x){
  3*x-15-exp(-x+6)
}

uniroot(g,c(0,10))$root

x <-seq(0,30,1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")




f <- function(x){
  1/x
}

uniroot(f,c(-10,-3))$root

uniroot(f,c(-1,1))$root

uniroot(f,c(-1,1))

x <-seq(-1,1,0.1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

#Solve x^2+18 =-9x
f <- function(x){
  x^2 +18 - (-9*x)
}
uniroot(f,c(-10,-5),tol=.0000001)


x <-seq(-10,10,1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")





