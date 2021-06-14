rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y1 <- data$y1
plot(x,y1)

x

m.best <- sum(x*y1)/sum(x^2)

L <- function(m,x,y){
  prod((1/sqrt(2*pi))*exp(-(y-m*x)^2/2))
}
logL <- function(m,x,y){
  log(prod((1/sqrt(2*pi))*exp(-(y-m*x)^2/2)))
}

x.val <- seq(-2,2,0.01)
y.L <- as.vector(lapply(x.val,FUN=L,x=x,y=y1))
y.logL <- as.vector(lapply(x.val,FUN=logL,x=x,y=y1))

par(mfrow=c(1,2))
par(mar=c(2.5,2.5,0.25,0.25))
plot(x.val,y.L,type='l')
abline(v=m.best,col=4)
plot(x.val,y.logL,type='l')
abline(v=m.best,col=4)

f <- function(x,b=0,m=1){
  b + m*x
}

x.in <- seq(-10,10,0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y1,type='p',pch=16)
lines(x.in,f(x.in,m=m.best),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')

