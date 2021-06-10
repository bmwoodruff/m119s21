library(data4led)
bulb <- led_bulb(1,seed = 0123)
bulb
ts <- bulb$hours
ys <- bulb$percent_intensity
length(ts)
length(ys)

sum((ys-100)*ts)
  
c.11 <- sum(ts^2)
c.12 <- sum(ts^3)
c.21 <- sum(ts^3)
c.22 <- sum(ts^4)
b.1 <- sum((ys-100)*ts)
b.2 <- sum((ys-100)*ts^2)
  
a2 <- (c.11*b.2 - c.21*b.1)/(c.11*c.22 - c.21*c.12)
a1 <- (b.1-c.12*a2)/c.11

a1
a2





rm(list=ls())
# Florida Hurricane Data (2000-2020)
FL <- c(4,4,8,8,6,8,2,8,7,4,8,6,4,3,3,4,5,7,4,7,13)

L <- function(lambda,x){
  # Remember x must be a whole number.
  prod((lambda^x/factorial(x))*exp(-lambda))
}

logL <- function(lambda,x){
  # Remember x must be a whole number.
  sum(log((lambda^x/factorial(x))*exp(-lambda)))
}


parm.l <- seq(0,10,0.001)
best.l <- sum(FL)/21

y.L <- as.numeric(lapply(parm.l,FUN=L,x=FL))
y.logL <- as.numeric(lapply(parm.l,FUN=logL,x=FL))

par(mfrow = c(1,2), mar=c(2.5,2.5,3,0.25))
plot(parm.l,y.logL,type='l',main='logLikelihood',ylim=c(-100,-40))
abline(v=best.l,col=2)
plot(parm.l,y.L,type='l',main='Likelihood')
abline(v=best.l,col=2)

best.l
mean(FL)
