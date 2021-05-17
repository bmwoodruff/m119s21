#Define the functions.
f1 <- function(x){x^4 -10*x^2 +3*x}
f2  <- function(x){exp(2*x)-1}
f3 <- function(x){sign(x-1)*(abs(x-1))^(1/3)}
f4 <- function(x){3*log(x-2)}

#Graph the functions.
x <- seq(-5,5,1e-3)

###figure 1###
y1 <- f1(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y1,type='l',xlim=c(-8,0))
points(-4,f1(-4),pch=16,col=2)
plot(x,y1,type='l',xlim=c(-5,-3),ylim=c(50,100))
points(-4,f1(-4),pch=16,col=2)
plot(x,y1,type='l',xlim=c(-4.2,-3.8),ylim=c(50,100))
points(-4,f1(-4),pch=16,col=2)
plot(x,y1,type='l',xlim=c(-4.1,-3.9),ylim=c(50,100))
points(-4,f1(-4),pch=16,col=2)


###figure 2###
y2 <- f2(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y2,type='l',xlim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-1,1),ylim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.5,0.5),ylim=c(-1,1))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.01,0.01),ylim=c(-0.25,0.25))
points(0,f2(0),pch=16,col=2)

###figure 3###
x3 <- seq(0,5,1e-3)
y3 <- f3(x3)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x3,y3,type='l',xlim=c(-5,5))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1,3),ylim=c(0,2))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.5,2.5),ylim=c(0,2))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.99,2.01),ylim=c(0.99,1.01))
points(2,f3(2),pch=16,col=2)

###figure 4###
x4 <- seq(2,10,1e-3)
y4 <- f4(x4)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x4,y4,type='l',xlim=c(0,10))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(1.75,3.75),ylim=c(-2,1))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.5,3),ylim=c(-2,1))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.7,2.8),ylim=c(-2,1))
points(2.75,f4(2.75),pch=16,col=2)


###figure 1###
y1 <- f1(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y1,type='l',xlim=c(-4,4))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2,4),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.5,3.5),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.9,3.1),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)



f4 <- function(x){3*log(x-2)}

dx <- 0.000001
c <- 2.75
x <- seq(c-5*dx,c+5*dx,dx)
f4(x)

data.frame(x,(f4(x)-f4(c))/(x-c))




m1 <- 51
m2 <- 2
m3 <- 1/3
m4 <- 4





#Define the linearization function.
t <- function(x,a,fa,df){
  fa + df*(x - a)
}

#Visualize these linearizations.
x <- seq(-5,5,1e-3)

###figure 1###
m1 <- 51
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x,f1(x),type='l',xlim=c(-4,4))
points(3,f1(3),pch=16,col=2)
lines(x,t(x,3,f1(3),m1),col=3)
plot(x,f1(x),type='l',xlim=c(2.9,3.1),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
lines(x,t(x,3,f1(3),m1),col=3)

x <- seq(-2,2,1e-3)

###figure 2###
m2 <- 2
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x,f2(x),type='l',xlim=c(-2,2))
points(0,f2(0),pch=16,col=2)
lines(x,t(x,0,f2(0),m2),col=3)
plot(x,f2(x),type='l',xlim=c(-0.01,0.01),ylim=c(-0.25,0.25))
points(0,f2(0),pch=16,col=2)
lines(x,t(x,0,f2(0),m2),col=3)


x <- seq(-5,5,1e-3)

###figure 3###
x3 <- seq(0,5,1e-3)
m3 <- 1/3
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x3,f3(x3),type='l',xlim=c(-5,5))
points(2,f3(2),pch=16,col=2)
lines(x3,t(x3,2,f3(2),m3),col=3)
plot(x3,f3(x3),type='l',xlim=c(1.99,2.01),ylim=c(0.99,1.01))
points(2,f3(2),pch=16,col=2)
lines(x3,t(x3,2,f3(2),m3),col=3)

###figure 4###
x4 <- seq(2,10,1e-3)
m4 <- 4
  par(mfrow=c(1,2),mar=c(2,2,0.25,0.25))
plot(x4,f4(x4),type='l',xlim=c(0,10))
points(2.75,f4(2.75),pch=16,col=2)
lines(x4,t(x4,2.75,f4(2.75),m4),col=3)
plot(x4,f4(x4),type='l',xlim=c(2.7,2.8),ylim=c(-2,1))
points(2.75,f4(2.75),pch=16,col=2)
lines(x4,t(x4,2.75,f4(2.75),m4),col=3)



###example 1###
x <- seq(-10,2,1e-3)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,abs(x+4),type='l',xlim=c(-9,1),ylim=c(-3,5))
points(-4,0,pch=16,col=2)
plot(x,abs(x+4),type='l',xlim=c(-6,-2),ylim=c(-3,3))
points(-4,0,pch=16,col=2)
plot(x,abs(x+4),type='l',xlim=c(-4.5,-3.5),ylim=c(-1,1))
points(-4,0,pch=16,col=2)
plot(x,abs(x+4),type='l',xlim=c(-4.01,-3.99),ylim=c(-0.01,0.01))
points(-4,0,pch=16,col=2)


###example 2###
f3 <- function(x){sign(x-1)*(abs(x-1))^(1/3)}
x3 <- seq(-5,5,1e-4)
y3 <- f3(x3)

par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x3,y3,type='l',xlim=c(-5,5))
points(1,f3(1),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(0,2),ylim=c(-1,1))
points(1,f3(1),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(0.5,1.5),ylim=c(-0.75,0.75))
points(1,f3(1),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(0.99,1.01),ylim=c(-0.01,0.01))
points(1,f3(1),pch=16,col=2)





