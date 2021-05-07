f <- function(x){2*x^2-4}
f(-2)
f(3)

y0 <- 12 #Something between 4 and 14
g <- function(x){f(x)-y0}

#find x0 so that f(x0) = y0
uniroot(g,c(-2,3))$root

#Plots to help us. 
x<-seq(-2,3,0.1)
plot(x,f(x),type="l")

g <- function(x){f(x)-y0}
x<-seq(-2,3,0.1)
plot(x,g(x),type="l")

uniroot(g,c(-2,3))$root





rm(list=ls())
f <- function(x){
  (-13 + 2*x)*exp(-0.05*x)
}
f(-10)
f(100)
x <- seq(-10,100,0.1)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x, f(x), type='l')
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)

uniroot(f,c(-10,100))$root


g <- function(x){f(x)-(-32)}
uniroot(g, c(-10,100))$root

par(mar=c(2.5,2.5,0.5,0.5))
plot(x, f(x), type='l')
abline(h=0,col='gray',lty=3)
abline(v=0,col='gray',lty=3)
abline(v=uniroot(g, c(-10,100))$root)
abline(h=-32)

















f
