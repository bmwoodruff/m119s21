rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed=2021)

t <- bulb$hours
y <- bulb$percent_intensity

c.11 <- sum(t^2)
c.12 <- sum(t^3)
c.22 <- sum(t^4)
b.1 <- sum((y-100)*t)
b.2 <- sum((y-100)*t^2)

c.11
c.12
c.22
b.1
b.2

best.a2 <- (c.11*b.2 - c.12*b.1)/(c.11*c.22 - c.12^2)
best.a1 <- (b.1 - c.12*best.a2)/c.11

best.a2
best.a1

D <- (-c.11)*(-c.22) - (-c.12)^2

D
-c.11


f2 <- function(x,a0=0,a1=0,a2=1){
  a0 + a1*x + a2*x^2
}

a0.2 <- 100
a1.2 <- best.a1
a2.2 <- best.a2

x <- seq(-10,800001,2)
par(mfrow=c(1,2),mar=c(4,4,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,f2(x,a0=a0.2,a1=a1.2,a2=a2.2),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x,a0=a0.2,a1=a1.2,a2=a2.2),col=2)


##Repeat for f6
rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed=2021)

t <- bulb$hours
y <- bulb$percent_intensity

c.11 <- sum(t^2)
c.12 <- sum(t*(1-exp(-0.0003*t)))
c.22 <- sum((1-exp(-0.0003*t))^2)
b.1 <- sum((y-100)*t)
b.2 <- sum((y-100)*(1-exp(-0.0003*t)))

c.11
c.12
c.22
b.1
b.2

best.a2 <- (c.11*b.2 - c.12*b.1)/(c.11*c.22 - c.12^2)
best.a1 <- (b.1 - c.12*best.a2)/c.11

best.a2
best.a1

D <- (-c.11)*(-c.22) - (-c.12)^2

D
-c.11


f6 <- function(x,a0=100,a1=0,a2=1){
  a0 + a1*x + a2*(1-exp(-0.0003*x))
}

a0.6 <- 100
a1.6 <- best.a1
a2.6 <- best.a2

x <- seq(-10,800001,2)
par(mfrow=c(1,2),mar=c(4,4,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f6')
lines(x,f6(x,a0=a0.6,a1=a1.6,a2=a2.6),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f6(x,a0=a0.6,a1=a1.6,a2=a2.6),col=2)



rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed=2021)

t <- bulb$hours
y <- bulb$percent_intensity
y
t




c.11 <- sum(t^2)
c.12 <- sum(t*(1-exp(-0.0003*t)))
c.22 <- sum((1-exp(-0.0003*t))^2)
b.1 <- sum((y-100)*t)
b.2 <- sum((y-100)*(1-exp(-0.0003*t)))


c.11
c.12
c.22
b.1
b.2


cPts <- function(c.11,c.12,c.22,b.1,b.2){
  best.y <- (c.11*b.2 - c.12*b.1)/(c.11*c.22 - c.12^2)
  best.x <- (b.1 - c.12*best.y)/c.11
  
  return(c(best.x,best.y))
}

a <- cPts(c.11,c.12,c.22,b.1,b.2)
a[1]
a[2]

D <- (-c.11)*(-c.22) - (-c.12)^2

-c.11
D

f6 <- function(x,a0=100,a1=0,a2=1){
  a0 + a1*x + a2*(1-exp(-0.0003*x))
}

a <- cPts(c.11,c.12,c.22,b.1,b.2)
a0.6 <- 100
a1.6 <- a[1]
a2.6 <- a[2]

x <- seq(-10,800001,2)
par(mfrow=c(1,2),mar=c(4,4,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f6')
lines(x,f6(x,a0=a0.6,a1=a1.6,a2=a2.6),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f6(x,a0=a0.6,a1=a1.6,a2=a2.6),col=2)

