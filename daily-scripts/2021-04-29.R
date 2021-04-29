x <- seq(-4,20,0.1)
f <- function(x){10-sqrt(x+4)}
y <- f(x)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')

f(1)


p <- function(x,a=-1,b=5){
  a*x+b
}

x <- seq(0,20,0.1)
plot(x,p(x,-1,1000),type='l')
abline(h=0,col='gray')
abline(v=0,col='gray')

p(20)
