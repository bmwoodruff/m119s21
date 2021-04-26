x <- seq(1,10,1)
f <- function(x){sqrt(x)}

  
y <- f(x)
plot(x,y,type = "l")    
plot(x,y)    


u <- c(3,4,5,6,10,-2)
class(u)
v <- c(2,"a")
class(v)

list(2,"a")


iris
x <- iris$Sepal.Width
y <- iris$Sepal.Length
plot(x,y)

?head
head(iris)

rm(list=ls())


f1 <- function(x){
  sqrt(3-x)
}

f1(3)
f1(0)
f1(-100)
f1(10)

x <- seq(-10,3,0.1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f1(x),type='l')
#?par
#??mar

f.c1 <- function(x){
  4
}

f.c1(-2)
f.c1(-1)
f.c1(0)
f.c1(1)
f.c1(2)

x <- seq(-6,6,2)
f.c1(x)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c1(x),type='l')
x
f.c1(x)


f.c2 <- function(x){
  4 + 0*x
}

f.c2(-2)
f.c2(-1)
f.c2(0)
f.c2(1)
f.c2(2)

f.c2(x)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c2(x),type='l')
x
f.c2(x)

c(1,2,3,4)
c(1,2,3,4)*0 +5


x <- seq(-10,10,1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c1(x),type='l')
#The one above breaks because f.c1 always return a single 4, even with a vector of inputs.


par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.c2(x),type='l')





f.quad <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f.quad(a=1,x=-2)
f.quad(-1)
f.quad(0)
f.quad(1)
f.quad(2)

x <- seq(-4,4,.1)
y <- f.quad(x)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='l')






f.quad <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f.quadA <- function(x,a,b,c){
  a*x^2 + b*x + c
}

f.quad(1/2)
f.quadA(1/2) #Note a,b,c are no longer optional. 
f.quadA(1/2,1,0,0)

f.quad(0,1,2,7)
f.quadA(0,1,2,7)

f.quad(-1/3)
f.quadA(-1/3,1,0,0)

my_fun <- function(x,a=3,b=2,c=1/2,d=-10) {c*(a*(x+b))^2+d}
x <- seq(-5,5,1)
y <- my_fun(x)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='l')


plot(seq(-5,5,1),my_fun(x,c=-10),type='l')





