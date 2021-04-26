#Remember you run each line by typing Cntl+Enter. 

2+3

3*3-2
3*(-1)-2
3*(6)-2

f <- function(x){3*x-2}
f(3)
f(-1)
f(6)

f2 <- function(x,y){4*x^2-5*y*sqrt(x+1) }
f2(3,2)
f2(0,-2.1)
f2(-2.1,5)

seq(1,100)

f3 <- function(x){
  ifelse(x>=0, x,-x)
}
f3(4)
f3(-10)
f3(0)


f4 <- function(x){
  ifelse(x < -1,x^3,
         ifelse(-1 < x & x < 4, -2,
                ifelse(x >= 4, sqrt(x),
                       NaN))
         )
}
 
f4(-2)
f4(-0.5)
f4(5.2)
f4(-1)

#for fun, let's plot it. 
x <- seq(-2,5,0.01)
y <- f4(x)

#Here is a prettied up plot. 
x <- seq(-2.5,36,0.01)
y <- f4(x)
par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
plot(x,y,type='l')
abline(v=0,lty=3,col='gray')
abline(h=0,lty=3,col='gray')
points(c(-1,-1,4),c(-1,-2,-2))
points(4,2,pch=16)

