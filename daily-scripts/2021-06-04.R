f <- function(x){x*exp(-x)}
Df <- function(x){1*exp(-x) - x*(exp(-x))}
D2f <- function(x){-2*exp(-x) + x*exp(-x)}

uniroot(Df,c(-10,10))$root
cv <- uniroot(Df,c(-10,10))$root
Df(cv)
Df(1)
signif(cv,5)
round(cv,6)
round(cv,5)

D2f(1)
D2f(cv)
#OR
-2*exp(-1) + 1*exp(-1)


x <- seq(-10,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l")

x <- seq(-1,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l")

x <- seq(0,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l")



my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}




par(mfrow=c(2,3))
my_plot(f,-10,10,0.001) #We specify plotting another point every 0.0001. 
my_plot(f,-10,10) #The default in our custom function uses 101 points. 
my_plot(f,-10,10,2) #We specify plotting another point every 2. 
my_plot(f,-1,10)
my_plot(f,-1,10,ylim=c(-3,1))
my_plot(f,0,10,ylim=c(0,0.5))


par(mfrow=c(1,1))
my_plot(f,0,3)


a<-0
b<-10
par(mfrow=c(1,1))
my_plot(f,a,b,ylim=c(-0.5,0.5))
my_lines(Df,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2f,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,f(cv)) #Add a black dot at the maximum
points(cv,Df(cv),col="red") #Add a red dot at the critical value with height zero (f' = 0)
points(cv,D2f(cv),col="green") #Add a green dot to show the value of the second dervaitive at the critical value. 













g <- function(x){x*(1-x)}
Dg <- function(x){1-2*x}
D2g <- function(x){0*x-2}

uniroot(Dg,c(-10,10))$root
cv <- uniroot(Dg,c(-10,10))$root
cv
Dg(cv) #The derivative is zero at this critical value. 

D2g(1/2)
D2g(cv)

my_plot(g,-2,2)
points(cv,g(cv))
cv #x value
g(cv) #Actual maximum height of this function. 


a <- -2
b <- 2
my_plot(g,a,b)
my_lines(Dg,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2g,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,g(cv))
points(cv,Dg(cv),col="red")
points(cv,D2g(cv),col="green")



h <- function(x){x^3-x}
Dh <- function(x){3*x^2-1}
D2h <- function(x){6*x}

my_plot(h,-2,2)

my_plot(Dh,-2,2)
abline(h=0)


my_plot(h,-2,2)
my_lines(Dh,-2,2,col="red")
abline(h=0, lty=2)

-sqrt(1/3)
sqrt(1/3)

cv <- uniroot(Dh,c(-10,10))$root
cv.1 <- uniroot(Dh,c(-10,0))$root
cv.2 <- uniroot(Dh,c(0,10))$root

cv.1
-sqrt(1/3)
signif(cv.1,5)
signif(-sqrt(1/3),5)
Dh(signif(-sqrt(1/3),5))
Dh(cv.1)
Dh(-sqrt(1/3))
cv.2
sqrt(1/3)

D2h(-sqrt(1/3))
D2h(cv.1)

D2h(sqrt(1/3))
D2h(cv.2)


a <- -2
b <- 2
my_plot(h,a,b)
my_lines(Dh,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2h,a,b, col = "green")
abline(v=cv.1,col="blue",lty = 2)
points(cv.1,h(cv.1))
points(cv.1,Dh(cv.1),col="red")
points(cv.1,D2h(cv.1),col="green")
abline(v=cv.2,col="blue",lty = 2)
points(cv.2,h(cv.2))
points(cv.2,Dh(cv.2),col="red")
points(cv.2,D2h(cv.2),col="green")



my_plot(h,-1,2)

h(-1)
h(-sqrt(1/3))
h(cv.1)
h(sqrt(1/3))
h(cv.2)
h(2)


cvs <- c(-1,-sqrt(1/3),cv.1, sqrt(1/3),cv.2,2)
cvs
h(cvs)
max(h(cvs))
min(h(cvs))

data.frame(x = cvs, y = h(cvs))

