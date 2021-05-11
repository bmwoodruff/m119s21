f5 <- function(x,a=0,b=1){
  
  out <- rep(0,length(x))
  out[(a <= x) & (x <= b)] <- (x[(a <= x) & (x <= b)]-a)/(b-a)
  out[(x > b)] <- 1
  
  return(out)
}

x <- seq(-1000,10000,2) #domain
y <- f5(x, 20,50)

par(mar=c(2,2,2,2))
plot(x,y,type = "l",ylim=c(-0,1))




f1 <- function(x,n=20,p=0.5){
  # x must be an whole number between 0 and n, endpoints included
  factorial(n)/(factorial(x)*factorial(n-x))*p^x*(1-p)^(n-x)
}
x <- seq(0,3,1) #domain
y <- f1(x,3,.2)

par(mar=c(2,2,2,2))
plot(x,y,ylim=c(-0,1),pch=16)




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



